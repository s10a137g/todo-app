/**
  * to do sample project
  */

package controllers.category

import javax.inject._
import play.api.mvc._

import lib.persistence.onMySQL.{CategoryRepository, TodoRepository}
import lib.model.Category
import model.ViewValueHome
import model.error.ViewValueError
import model.category.{
  ViewValueCategory,
  ViewValueCategoryEdit,
  ViewValueCategoryList
}
import lib.model.Todo
import slick.jdbc.JdbcProfile
import ixias.persistence.SlickRepository
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.concurrent.duration._

import play.api.data._
import play.api.data.Forms._

import play.api.i18n.I18nSupport
import play.api.data.validation.Constraint
import play.api.data.validation
import play.api.data.validation.{Invalid, Valid, ValidationError}

// Category追加フォーム
case class CategoryInsertFormData(name: String, slug: String, color: Short)

//  Category更新フォーム
case class CategoryUpdateFormData(
  id:    Long,
  name:  String,
  slug:  String,
  color: Short
)

@Singleton
class CategoryController @Inject() (
  val controllerComponents: ControllerComponents
) extends BaseController
     with I18nSupport {

  val numericAlphaChar = """^[0-9a-zA-Z]*$""".r

  val slugCheckConstraint: Constraint[String] =
    Constraint("constraints.slugcheck")({ plainText =>
      val errors = plainText match {
        case numericAlphaChar() => Nil
        case _                  => Seq(ValidationError("スラッグは英数字のみ入力可能です。"))
      }

      if (errors.isEmpty) Valid else Invalid(errors)
    })

  val slugCheck: Mapping[String] = nonEmptyText.verifying(slugCheckConstraint)

  val categoryInsertForm = Form(
    mapping(
      "name"  -> nonEmptyText,
      "slug"  -> slugCheck,
      "color" -> shortNumber
    )(CategoryInsertFormData.apply)(CategoryInsertFormData.unapply)
  )

  val categoryUpdateForm = Form(
    mapping(
      "id"    -> longNumber,
      "name"  -> nonEmptyText,
      "slug"  -> slugCheck,
      "color" -> shortNumber
    )(CategoryUpdateFormData.apply)(CategoryUpdateFormData.unapply)
  )

  def list = Action.async { implicit req =>
    for {
      categoryList <- CategoryRepository.getAll()
    } yield {
      val viewValueCategory = categoryList.map(i =>
        ViewValueCategory(i.id.toLong, i.v.name, i.v.slug, i.v.color)
      )
      Ok(
        views.html.category.list(
          ViewValueCategoryList(
            "Category List",
            cssSrc = Seq("main.css"),
            jsSrc  = Seq("main.js"),
            viewValueCategory
          )
        )
      )
    }
  }

  def displayInsert = Action { implicit req =>
    Ok(
      views.html.category.insert(
        ViewValueCategoryEdit(
          "Category Add",
          Seq("main.css"),
          Seq("main.js")
        ),
        categoryInsertForm
      )
    )
  }

  def insert = Action.async { implicit req =>
    //Formバリデーションエラー判定
    categoryInsertForm
      .bindFromRequest().fold(
        // エラー時遷移
        (formWithErrors: Form[CategoryInsertFormData]) => {
          Future {
            BadRequest(
              views.html.category.insert(
                ViewValueCategoryEdit(
                  "Category Add",
                  Seq("main.css"),
                  Seq("main.js")
                ),
                formWithErrors
              )
            )
          }
        },
        // 正常時遷移
        (f: CategoryInsertFormData) => {
          for {
            result <- CategoryRepository.add(
              Category(f.name, f.slug, Category.Color(f.color))
            )
          } yield {
            Redirect(controllers.category.routes.CategoryController.list)
          }
        }
      )
  }

  def displayUpdate(id: Long) = Action.async { implicit req =>
    for {
      maybeUpdateCategory <- CategoryRepository.get(Category.Id(id)),
    } yield maybeUpdateCategory match {
      case None                 =>
        BadRequest(
          views.html.error.error(
            ViewValueError("Category Add", Seq("main.css"), Seq("main.js"))
          )
        )
      case Some(updateCategory) =>
        val inputMap = Map(
          "id"    -> updateCategory.id.toString,
          "name"  -> updateCategory.v.name,
          "slug"  -> updateCategory.v.slug,
          "color" -> updateCategory.v.color.code.toString
        )
        Ok(
          views.html.category.update(
            ViewValueCategoryEdit(
              "Category Update",
              Seq("main.css"),
              Seq("main.js")
            ),
            categoryUpdateForm.bind(inputMap)
          )
        )
    }
  }

  def update() = Action.async { implicit req =>
    categoryUpdateForm
      .bindFromRequest().fold(
        (formWithErrors: Form[CategoryUpdateFormData]) => {
          Future {
            BadRequest(
              views.html.category.update(
                ViewValueCategoryEdit(
                  "Category Add",
                  Seq("main.css"),
                  Seq("main.js")
                ),
                formWithErrors
              )
            )
          }
        },
        (data: CategoryUpdateFormData) => {
          val category = Category.build(
            Category.Id(data.id),
            data.name,
            data.slug,
            Category.Color(data.color)
          )

          for {
            result <- CategoryRepository.update(category)
          } yield result match {
            case Some(v) =>
              Redirect(controllers.category.routes.CategoryController.list)
            case None    =>
              BadRequest(
                views.html.error.error(
                  ViewValueError(
                    "Category Add",
                    Seq("main.css"),
                    Seq("main.js")
                  )
                )
              )
          }
        }
      )
  }

  def delete(id: Long) = Action.async { implicit req =>
    (for {
      categoryResult <- CategoryRepository.remove(Category.Id(id))
      todoResult     <- TodoRepository.removeByCategoryId(Category.Id(id))
    } yield {
      Redirect(controllers.category.routes.CategoryController.list)
    }) recover {
      case e =>
        BadRequest(
          views.html.error.error(
            ViewValueError(
              "Category Add",
              Seq("main.css"),
              Seq("main.js")
            )
          )
        )
    }
  }
}
