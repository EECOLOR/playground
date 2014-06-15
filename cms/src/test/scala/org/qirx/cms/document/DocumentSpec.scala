package org.qirx.cms.document

import org.qirx.littlespec.Specification
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.macros.Location
import play.api.libs.json.Json
import play.api.libs.json.Json.obj
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import scala.language.dynamics
import scala.language.reflectiveCalls
import play.api.libs.json.Writes
import scala.language.implicitConversions

object DocumentSpec extends Specification {

  private def idLowerCase(kind: String, id: String) =
    require(id.toLowerCase == id, s"The $kind id should be lowercase")

  private def nameUnique(kind: String, seq: Seq[String]) =
    require(seq.toSet.size == seq.size, s"The id's of $kind must be unique")

  case class DocumentInstance(id: String, properties: Map[String, PropertyInstance])
  case class PropertyInstance(property: Property, value: JsValue)

  case class Documents(documents: Seq[Document]) {
    val documentIds = documents.map(_.id)
    val documentsById = documentIds.zip(documents).toMap

    nameUnique("documents", documentIds)

    def jsonReaderFor(id: String): Option[Reads[DocumentInstance]] =
      documentsById.get(id).map(_.jsonReader)
  }

  case class Document(id: String)(val properties: (String, PropertyLike)*) {
    val propertyIds = properties.map { case (id, _) => id }

    idLowerCase("document", id)
    nameUnique("properties", propertyIds)

    def jsonReader: Reads[DocumentInstance] = ???
  }

  trait PropertyLike {
    val id: String
    val isRequired: Boolean
    val isGenerated: Boolean
  }

  class GeneratedValueProperty(property: Property with Generatable) extends PropertyLike {
    val id = property.id
    val isRequired = false
    val isGenerated = true
  }

  trait Generatable { self: Property =>
    def generated = new GeneratedValueProperty(self)
  }

  class OptionalProperty(property: Property) extends PropertyLike {
    val id = property.id
    val isRequired = false
    val isGenerated = false
  }

  class Property(val id: String) extends PropertyLike {
    idLowerCase("property", id)

    val isRequired = true
    val isGenerated = false

    def ? = new OptionalProperty(this)
  }

  class Label(id: String) extends Property(id)
  object Label extends Label("label")

  class RichContent(id: String) extends Property(id)
  object RichContent extends RichContent("richcontent")

  class LatLon(id: String) extends Property(id)
  object LatLon extends LatLon("latlon")

  trait Identifyable { self =>
    private def set(nonEmpty: Boolean) = Set(self, nonEmpty = true)
    def + = set(nonEmpty = true)
    def * = set(nonEmpty = false)
  }
  class Set(id: String, property: Identifyable, nonEmpty: Boolean = false) extends Property(id)
  object Set {
    def apply(property: Identifyable, nonEmpty: Boolean = false) =
      new Set("set", property, nonEmpty)
  }
  class Choice(id: String, options: Seq[String]) extends Property(id) with Identifyable {
    require(!options.contains(""), "Items can not contain an empty string")
    nameUnique("options", options)
  }
  object Choice {
    def apply(options: String*) = new Choice("choice", options)
  }
  class Tag(id: String) extends Property(id) with Identifyable
  object Tag extends Tag("tag")

  class DocumentRef(id: String, document: Document) extends Property(id) with Identifyable
  class EnumerationRef(id: String, enumeration: Enumeration) extends Property(id) with Identifyable
  class TreeEnumerationRef(id: String, tree: EnumerationTree) extends Property(id) with Identifyable
  object Ref {
    def apply(document: Document) = new DocumentRef("document", document)
    def apply(enumeration: Enumeration) = new EnumerationRef("enumeration", enumeration)
    def apply(tree: EnumerationTree) = new TreeEnumerationRef("tree", tree)
  }
  object Date extends Property("date") with RangeFunctions with Generatable
  trait RangeFunctions { self: Property =>
    def range = Range(self)
  }
  class Range(id: String, property: Property with RangeFunctions) extends Property(id)
  object Range {
    def apply(property: Property with RangeFunctions) = new Range("range", property)
  }
  case object Number extends Property("number") with RangeFunctions

  class Enumeration(id: String, values: Seq[Element]) {
    idLowerCase("enumeration", id)
  }
  object Enumeration {
    def apply[T](id: String)(values: T*)(implicit toElement: T => Element) =
      new Enumeration(id, values.map(toElement))
  }
  class EnumerationTree(id: String, values: Tree[Properties => JsValue]) {
    idLowerCase("tree enumeration", id)
  }
  object EnumerationTree {
    def mapNodes[T](nodes: Seq[(String, Node[T])])(
      implicit toGetValue: T => Properties => JsValue): Seq[(String, Node[Properties => JsValue])] = {
      nodes.map {
        case (key, Leaf(value)) => key -> Leaf(toGetValue(value))
        case (key, branch: Tree[T]) => key -> Tree(mapNodes(branch.nodes))
      }
    }

    def apply[T](id: String)(values: Tree[T])(implicit toGetValue: T => Properties => JsValue) = {
      val elementTree = Tree(mapNodes(values.nodes))
      new EnumerationTree(id, elementTree)
    }
  }

  case class Element(id: String, value: Option[Properties => JsValue]) {
    idLowerCase("enumeration", id)

    def getValue(properties: Properties): JsValue =
      value.map(_ apply properties).getOrElse(id)
  }
  object Element {
    implicit def pairToElement[T](pair: (String, Properties => T))(implicit toJson: Writes[T]): Element = {
      val (id, value) = pair
      val wrapped = (p: Properties) => toJson.writes(value(p))
      Element(id, Some(wrapped))
    }

    implicit def idToElement(id: String): Element =
      Element(id, None)
  }

  trait Properties {
    def apply(s: String): String
  }

  class File(id: String) extends Property(id) with Identifyable
  object File extends File("file")

  class Image(id: String) extends Property(id) with Identifyable
  object Image extends Image("image")

  sealed trait Node[A]
  object Node {
    implicit def toNode[A](a:A):Node[A] = Leaf(a)
  }
  case class Leaf[A](value: A) extends Node[A]
  case class Tree[A](nodes: Seq[(String, Node[A])]) extends Node[A]

  implicit def toLeaf[T](pair: (String, T)): (String, Node[T]) = {
    val (key, value) = pair
    key -> Leaf(value)
  }

  def tree[T](nodes: (String, Node[T])*): Tree[T] = Tree(nodes)

  "properties should derive their id from their class name" - {}

  "automatic versioning by comparing document metadata" - {}

  "Documents" - {

    new ExampleWithDocuments {
      case object Age extends Range("age", Number)

      val menu = {
        val link = { (properties: Properties) =>
          Map(
            "url" -> properties("url"),
            "label" -> properties("label")
          )
        }

        Enumeration(id = "menu")(
          "events" -> link,
          "articles" -> link
        )
      }

      val kind =
        Enumeration(id = "kind")(
          "fact",
          "fiction"
        )

      val category = {
        def item(featured: Boolean = false) = { (properties: Properties) =>
          obj(
            "label" -> properties("label"),
            "featured" -> featured
          )
        }

        EnumerationTree(id = "category")(
          tree(
            "music" -> tree(
              "rock" -> item(featured = true),
              "hippy" -> item()
            ),
            "movies" -> item(featured = true)
          )
        )
      }

      val enumerations = Seq(menu, kind, category)

      val article =
        Document(id = "article")(
          "title" -> Label,
          "body" -> RichContent,
          "category" -> Ref(category),
          "tags" -> Tag.*,
          "kind" -> Ref(kind),
          "attachments" -> File.*
        )

      val news =
        Document(id = "news")(
          "title" -> Label,
          "image" -> Image,
          "summary" -> RichContent,
          "body" -> RichContent,
          "kind" -> Ref(kind),
          "category" -> Ref(category),
          "date" -> Date.generated
        )

      class TreeChoice(nodes:Tree[String]) extends Property("tree")
      object TreeChoice {
        def apply(nodes:Seq[(String, Node[String])]) = 
          new TreeChoice(Tree(nodes))
      }
        
      def TreeChoice[T, N](nodes: (String, N)*)(implicit asNode: N => Node[T]) =
        ??? //Node.toBranch(nodes)
      val event =
        Document(id = "event")(
          "title" -> Label,
          "body" -> RichContent,
          "location" -> LatLon.?,
          "environment" -> Choice("indoor", "outdoor"),
          "tags" -> Tag.*,
          "article" -> Ref(article).+,
          "period" -> Date.range,
          "tickets" -> Number,
          "category" -> TreeChoice(
            "festival" -> tree(
              "rock" -> tree(
                "lowlands",
                "pinkpop"
              ),
              "hippy" -> tree(
                "woodstock",
                "isleofwight"
              ),
              "concert"
            )

          ),
          "minimumAge" -> Age
        )

      val documents = Seq(article, event)

    } withSpecification { example =>

      val documents = Documents(example.documents)

      "should be able to construct from json" - {

        val json =
          """{ 
            "title" : "test",
            "body": "body",
            "location": {
              "lat" : 12.34,
              "lon" : 56.789
            }
          }"""

        val documentReader = documents.jsonReaderFor("article")

        val document = readDocument using documentReader from json

        val expected = DocumentInstance(
          id = "article",
          properties = Map(
            "title" -> PropertyInstance(Label, "test")))

        document is expected
      }
    }

    mustHaveLowerCaseId(Document("Test")())
  }

  "A property" - {
    mustHaveLowerCaseId(new Property("Test") {})
  }

  "Item ids may not be empty" - {
    Choice("") must beIllegal
  }

  "Item ids must be unique" - {
    Choice("a", "a") must beIllegal
  }

  "Documents property names must be unique" - {
    Documents(Seq(Document("a")("b" -> Tag, "b" -> Tag))) must beIllegal
  }
  "Documents names must be unique" - {
    Documents(Seq(Document("a")(), Document("a")())) must beIllegal
  }

  val beIllegal = throwAn[IllegalArgumentException]

  implicit def anyToJsValue[T](t: T)(implicit toJson: Writes[T]): JsValue =
    toJson.writes(t)

  object readDocument {
    def using(reader: Option[Reads[DocumentInstance]]) = new {
      def from(json: String) = reader.get.reads(json.parse)
    }
  }

  implicit class StringToJson(s: String) {
    def parse = Json.parse(s)
  }

  private def mustHaveLowerCaseId(instance: => Any) =
    "must have a lower case id" - {
      instance must throwAn[IllegalArgumentException]
    }

  abstract class ExampleWithDocuments(implicit location: Location) { self =>
    def withSpecification(spec: self.type => FragmentBody) =
      createFragment(Source.codeAtLocation(location), spec(self))
  }
}