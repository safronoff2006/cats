import cats.data.Kleisli
import cats.implicits._

import scala.language.postfixOps
import scala.util.Try


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("KLeisli")

  object part1 {
    def chapter1 = {
      // https://medium.com/@supermanue/real-world-applications-of-scala-kleisli-configuration-ba39e97befec

      // Реальные приложения Scala Kleisli: конфигурация

      // Это третья статья из небольшой серии о Kleisli в реальном мире.
      // На этот раз мы увидим, как его можно использовать для чтения и передачи конфигурации.
      // Фактически, мы используем это в микросервисе, где параметры конфигурации считываются от пользователя,
      // и некоторые вещи могут давать сбой, поэтому необходимо обернуть инициализацию определенных
      // частей программного обеспечения в монаду Try.

      // Этот пример в основном взят из документации Cats Kleisli,
      // хотя с моей скромной точки зрения с дополнительными пояснениями того, что именно делает код,
      // и упрощен путем удаления некоторых менее важных деталей.
      // Также, для легкого введения в Kleisli, не стесняйтесь читать мой пост Understanding Kleisli in Scala.
      // https://medium.com/@supermanue/understanding-kleisli-in-scala-9c42ec1a5977


      //Это упрощение нашей БД. Оно включает класс конфигурации DbConfig с требуемыми параметрами
      // и саму БД, представленную трейтом, поэтому мы можем изменить реализацию в любой момент.
      // DBImpl.fromDbConfig пытается создать новый экземпляр БД с параметрами конфигурации.


      case class DbConfig(url: String, user: String, pass: String)

      trait DbInterface

      case class Db(name: String, url: String, userpwd: String) extends DbInterface

      object DbImpl {
        val fromDbConfig: Kleisli[Try, DbConfig, DbInterface] = Kleisli{
          config => Try {
            Db("Db_1", config.url, s"${config.user}@${config.pass}")
          }
        }
      }

      // То же самое и с Service, который представляет собой некую разновидность REST-службы,
      // которую мы собираемся использовать в нашем приложении.

      case class ServiceConfig(addr: String, port: Int)

      trait ServiceInterface

      case class Service(name: String, socket: String) extends ServiceInterface

      object ServiceImpl {
        val fromServiceConfig: Kleisli[Try, ServiceConfig, ServiceInterface] = Kleisli {
          config => Try {
            Service("Capture", s"${config.addr}:${config.port}")
          }
        }
      }

      // Это наше приложение. Оно имеет БД и сервис.
      // Поскольку это трейты, мы можем позже решить, как именно это реализовать.

      case class AppConfig(url: String, user: String, pass: String, addr: String, port: Int)

      case class App(db: DbInterface, service: ServiceInterface)

      // Мы определяем функцию, которая получит AppConfig и попытается инициализировать App.
      // В зависимости от результата она вернет Success(App) или Failure.

      // Почему мы используем Kleisli здесь, а не просто делаем что-то вроде appFromAppConfig(conf: AppConfig): Try[App]
      // и разрешаем db и sv, вызывая их с conf внутри for comprehension ?
      // Ну, это будет работать правильно, но если мы можем работать на более высоком уровне и
      // позволить мощной системе типов Scala сделать работу за нас, почему бы не рискнуть этой возможностью?

      def appFromAppConfig: Kleisli[Try, AppConfig, App] =
        // Обратите внимание, что for comprehension работает на Kleisli. Круто!
        for {
          // Здесь мы используем local. Это мощная функция, которая позволяет использовать другой тип входа для Kleisli,
          // если вы сообщите ей, как преобразовать этот тип в тот который принимает Kleisli.
          // Здесь мы создаем анонимную функцию, которая берет appConfig и создает DbConfig, извлекая соответствующие параметры.
          db <- DbImpl.fromDbConfig.local[AppConfig](appConfig => DbConfig(appConfig.url, appConfig.user, appConfig.pass))
          sv <- ServiceImpl.fromServiceConfig.local[AppConfig](appConfig => ServiceConfig(appConfig.addr, appConfig.port))
        } yield App(db, sv)

      // Простой пример.
      val appConfig: AppConfig = AppConfig("url", "user", "pass", "addr", 80)
      val app: Try[App] = appFromAppConfig(appConfig)

      println{
        app
      }

    }
  }

  part1.chapter1

}


