import $ivy.`com.github.pathikrit:better-files_2.11:3.4.0`
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import better.files._
import scala.util._
import scala.xml._

// wget http://192.168.100.8:49153/setup.xml
// wget http://192.168.100.8:49153/eventservice.xml
// wget http://192.168.100.8:49153/metainfoservice.xml
// wget http://192.168.100.8:49153/deviceinfoservice.xml
// wget http://192.168.100.8:49153/insightservice.xml
// wget http://192.168.100.8:49153/manufacture.xml

@doc("Regenerate the wsdl for a belkin wemo")
@main
def refreshWsdl(ip: String @doc("ip address that wemo is available at")) = {
  type ParsedElement = (Seq[NodeSeq], Seq[NodeSeq], NodeSeq, NodeSeq)

  def buildActions(actions:  NodeSeq): Seq[(String, Seq[String])] = {
    actions.map { action =>
      (
        (action \ "name") text,
        (action \ "argumentList" \ "argument" \ "name") map (_ text)
      )
    }
  }

  def buildElement(elem: (String, Seq[String])): ParsedElement = {
    val items = elem._2.map { e =>
      s"""<xsd:element name="${e}" type="xsd:string"/>"""
    }.mkString("\n\t\t")


    val types = Seq(XML.loadString(s"""
      <xsd:element name="${elem._1}">
        <xsd:complexType>
          <xsd:all>
            ${items}
          </xsd:all>
        </xsd:complexType>
      </xsd:element>""".stripMargin),
    XML.loadString(s"""<xsd:element name="${elem._1}Response">
        <xsd:complexType>
          <xsd:all>
            ${items}
          </xsd:all>
        </xsd:complexType>
      </xsd:element>
    """.stripMargin))

    val message = Seq(XML.loadString(s"""
      <wsdl:message name="${elem._1}InputMessage">
          <wsdl:part name="${elem._1}InputPart" element="u:${elem._1}"/>
      </wsdl:message>""".stripMargin),
      XML.loadString(s"""<wsdl:message name="${elem._1}OutputMessage">
          <wsdl:part name="${elem._1}OutputPart" element="u:${elem._1}Response"/>
      </wsdl:message>
      """.stripMargin))

    val portType = XML.loadString(s"""
      <wsdl:operation name="${elem._1}">
        <wsdl:input message="${elem._1}InputMessage"/>
        <wsdl:output message="${elem._1}OutputMessage"/>
      </wsdl:operation>
      """.stripMargin)

    val binding = XML.loadString(s"""
      <wsdl:operation name="${elem._1}">
        <soap:operation soapAction="urn:Belkin:service:basicevent:1#${elem._1}"/>
        <wsdl:input>
          <soap:body use="literal"/>
        </wsdl:input>
        <wsdl:output>
          <soap:body use="literal"/>
        </wsdl:output>
      </wsdl:operation>
      """.stripMargin)

    (types, message, portType, binding)
  }

  def url(endpoint: String) = s"http://${ip}:49153/${endpoint}"
  val files = Seq(
    // "setup.xml",
    "eventservice.xml"
    // ,
    // "metainfoservice.xml",
    // "deviceinfoservice.xml",
    // "insightservice.xml",
    // "manufacture.xml"
  )

  import sys.process._

  //  todo, special wsdls
  files.map { f => 
    //remove old definitions
    rm(pwd/f)
    //refetch the file
    Seq(
      "wget",
      url(f)
    ).!

    val xml = XML.loadFile(f)
    val actions = xml \ "actionList" \ "action"
    val parsedElements = buildActions(actions).map(buildElement)

    val (types, messages, operations, bindings) = (
      parsedElements.map(_._1.mkString("\n")).mkString("\n"),
      parsedElements.map(_._2.mkString("\n")).mkString("\n"),
      parsedElements.map(_._3).mkString("\n"),
      parsedElements.map(_._4).mkString("\n"))

    val body = s"""
      <wsdl:definitions xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
                        xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
                        xmlns:u="urn:Belkin:service:basicevent:1">
          <wsdl:types>
              <xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" targetNamespace="urn:Belkin:service:basicevent:1">
              ${types}
              </xsd:schema>
          </wsdl:types>

          ${messages}
          <wsdl:portType name="BasicServicePortType">
              ${operations}
          </wsdl:portType>

          <wsdl:binding name="BasicServiceBinding" type="BasicServicePortType">
              <soap:binding transport="http://schemas.xmlsoap.org/soap/http"/>
              ${bindings}
          </wsdl:binding>

          <wsdl:service name="BasicService">
              <wsdl:port name="BasicServicePort" binding="BasicServiceBinding">
                  <soap:address location="http://${ip}:49153/upnp/control/basicevent1"/>
              </wsdl:port>
          </wsdl:service>
      </wsdl:definitions>
      """.stripMargin

    val p = new PrettyPrinter(100, 2)
    // val out = file"BasicService-${f.replace(".xml", "")}.wsdl"
    val out = file"BasicService.wsdl"
    out.append(p.format(XML.loadString(body)))
  }
}
