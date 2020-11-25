package lib.model

import ixias.model._
import ixias.util.EnumStatus

import java.time.LocalDateTime

// カテゴリーを表すモデル
//~~~~~~~~~~~~~~~~~~~~
import Category._
case class Category(
  id:        Option[Id],
  name:      String,
  slug:      String,
  color:     Color,
  updatedAt: LocalDateTime = NOW,
  createdAt: LocalDateTime = NOW
) extends EntityModel[Id]

// コンパニオンオブジェクト
//~~~~~~~~~~~~~~~~~~~~~~~~
object Category {

  val Id = the[Identity[Id]]
  type Id         = Long @@ Category
  type WithNoId   = Entity.WithNoId[Id, Category]
  type EmbeddedId = Entity.EmbeddedId[Id, Category]

  // Color定義
  //~~~~~~~~~~~~~~~~~
  sealed abstract class Color(val code: Short, val name: String)
    extends EnumStatus
  object Color extends EnumStatus.Of[Color] {
    case object RED     extends Color(code = 1, name = "RED")
    case object YELLOW  extends Color(code = 2, name = "YELLOW")
    case object GREEN   extends Color(code = 3, name = "GREEN")
    case object WHITE   extends Color(code = 4, name = "WHITE")
    case object SILVER  extends Color(code = 5, name = "SILVER")
    case object GRAY    extends Color(code = 6, name = "GRAY")
    case object BLACK   extends Color(code = 7, name = "BLACK")
    case object MAROON  extends Color(code = 8, name = "MAROON")
    case object OLIVE   extends Color(code = 9, name = "OLIVE")
    case object LIME    extends Color(code = 10, name = "LIME")
    case object AQUA    extends Color(code = 11, name = "AQUA")
    case object TEAL    extends Color(code = 12, name = "TEAL")
    case object BLUE    extends Color(code = 13, name = "BLUE")
    case object NAVY    extends Color(code = 14, name = "NAVY")
    case object FUCHSIA extends Color(code = 15, name = "FUCHSIA")
    case object PURPLE  extends Color(code = 16, name = "PURPLE")
  }
  // INSERT時のIDがAutoincrementのため,IDなしであることを示すオブジェクトに変換
  def apply(
    name:  String,
    slug:  String,
    color: Color
  ): WithNoId = {
    new Entity.WithNoId(
      new Category(
        id    = None,
        name  = name,
        slug  = slug,
        color = color
      )
    )
  }

  def build(
    id:    Id,
    name:  String,
    slug:  String,
    color: Color
  ): EmbeddedId = {
    new Entity.EmbeddedId(
      new Category(
        id    = Some(id),
        name  = name,
        slug  = slug,
        color = color
      )
    )
  }
}
