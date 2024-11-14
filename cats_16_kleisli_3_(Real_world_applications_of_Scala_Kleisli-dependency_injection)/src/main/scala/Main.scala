import cats.data.Kleisli
import cats.implicits._

import cats.effect.{IO, Sync}
import scala.util.{Success, Try}

import cats.effect.unsafe.implicits._
import scala.language.postfixOps


object Main extends App {

  println("Изучаем Cats!".toUpperCase)
  println("KLeisli")

  object part1 {
    def chapter1 = {
      // https://medium.com/@supermanue/real-world-applications-of-scala-kleisli-dependency-injection-36ef589ee77b

      // Реальные приложения Scala Kleisli: внедрение зависимостей

      // После предыдущего введения в Kleisli https://medium.com/@supermanue/understanding-kleisli-in-scala-9c42ec1a5977 ,
      // в этой статье мы рассмотрим, как его можно использовать для внедрения зависимостей.
      // Как обычно, я сначала покажу немного кода, а затем опишу его построчно.
      // Кстати, если вы не знаете, что такое внедрение зависимостей, проверьте это объяснение.
      // https://www.freecodecamp.org/news/a-quick-intro-to-dependency-injection-what-it-is-and-when-to-use-it-7578c84fa88f/

      //Приложение "Отгрузка"

      // Это класс, с которым мы будем работать.
      // Он представляет простую Отгрузку, со ссылкой и статусом
      case class Shipment(ref: String, status: String)

      // Грузы хранятся в хранилище груза. Это очень простой трейт: у него есть только метод, который по ссылке отгрузки
      // извлекает его из хранилища и возвращает пользователю.
      // Обратите внимание, что это тип более высокого рода, поэтому мы можем вернуть Try, Either или что-то еще.
      trait ShipmentStorage[F[_]] {
        def retrieveShipment(shipmentReference: String): F[Shipment]
      }

      // Здесь у нас есть три различных реализации для этого трейта.
      // В реальном мире одна из них будет доступом к БД, другая вызовом REST и т. д.,
      // и у них будут входные параметры и аргументы.

      // Кроме того, все они используют разные монады для типа высшего порядка.
      // Более того, второй даже не определяет монаду, а только некоторые ограничения на нее.

      class ShipmentStorageImpl1 extends ShipmentStorage[IO]{
        override def retrieveShipment(shipmentReference: String): IO[Shipment] = IO.pure(Shipment(shipmentReference, "OK"))
      }

      class ShipmentStorageImpl2[F[_]: Sync] extends ShipmentStorage[F]{
        override def retrieveShipment(shipmentReference: String): F[Shipment] = Sync[F].delay(Shipment(shipmentReference, "OK"))
      }

      class ShipmentStorageImpl3 extends ShipmentStorage[Try]{
        override def retrieveShipment(shipmentReference: String): Try[Shipment] = Success(Shipment(shipmentReference, "OK"))
      }

      // Это псевдоним для Kleisli. Он представляет собой операцию над ShipmentStorage.
      // Обратите внимание, что и Kleisli, и ShipmentStorage используют один и тот же F.

      // Что это значит на самом деле? Ну, функция, возвращающая Operation[F, R],
      // на самом деле будет возвращать другую функцию, которая,
      // учитывая ShipmentStorage[F], возвращает F[R].
      // Это звучит немного абстрактно, но мы скоро увидим, насколько это просто в использовании и насколько это мощно.

      type Operation[F[_], R] = Kleisli[F, ShipmentStorage[F], R]

      // Это объект, где мы реализуем нашу бизнес-логику.
      // Я предоставил реализацию одного метода.

      object OperationService {

        // Вот где происходит магия. В этой функции мы хотим получить груз из ShipmentStorage и сделать с ним что угодно.
        // Здесь не нужно привязываться к определенному хранилищу, а использовать предоставленный трейт.

        // Используя Kleisli, мы возвращаем здесь не результат (Shipment),а Operation.
        // Поэтому, как мы видели в строке 68, при получении ShipmentStorage возвращает F[Shipment].

        // Также обратите внимание, что у этой функции есть дженерик параметр F[_]
        // Это позволяет сделать ее полиморфной, а затем указать, какой будет F во время выполнения.
        // Таким образом, функция независима от монады, используемой ShipmentStorage

        // Как видите, здесь Kleisli заботится о внедрении зависимости:
        // он вызывает retrieveShipment ShipmentRepository, полученного в качестве входного параметра,
        // что позволяет абстрагироваться от него.

        def getShipment[F[_]](shipmentReference: String): Operation[F, Shipment] =
          Kleisli{ shipmentStorage: ShipmentStorage[F] =>
            //логгирование данных перед доступом
            shipmentStorage.retrieveShipment(shipmentReference)
            //результат обработки после получения доступа
          }

        def storeShipment[F[_]](shipmentReference: String): Operation[F, Shipment] = ???

        def deleteShipment[F[_]](shipmentReference: String): Operation[F, Shipment] = ???

      }

      // Здесь мы видим результат всего этого: создаются разные OperationService с разными внедренными ShipmentStorage.

      // Все они работают одинаково, поэтому давайте рассмотрим первый случай:

      // - мы создаем экземпляр ShipmentStorageImpl1, который реализует ShipmentStorage.

      // - В нашем OperationService, который является синглтоном, мы хотим выполнить getShipment,
      // извлекая отгрузку из определенного хранилища.
      // Мы знаем, что getShipments требует ссылку на груз в качестве входного параметра и возвращает Operation,
      // который требует ShipmentStorage.
      // Мы предоставляем оба — вуаля!
      // Мы получаем IO[Shipment] из выбранной реализации нашего трейта ShipmentStorage.

      // - Как вы можете видеть в строке 129, вы можете захотеть сделать вызов метода Kleisli run явным.
      // Если нет, то Klesili apply() сделает это за вас, так что обе альтернативы (строки 121 и 129)
      // полностью эквивалентны

      val shipmentToProcess = "1234"
      val storage1 = new ShipmentStorageImpl1()
      val res = OperationService.getShipment(shipmentToProcess)(storage1).unsafeRunSync()
      //res: Shipment(1234, OK)

      println {
        res
      }

      val storage2 = new ShipmentStorageImpl2[IO]()
      val res2 = OperationService.getShipment(shipmentToProcess).run(storage2).unsafeRunSync()
      //res: Shipment(1234, OK)

      println {
        res2
      }

      val storage3 = new ShipmentStorageImpl3()
      val res3 = OperationService.getShipment(shipmentToProcess)(storage3)
      //res: Success(Shipment(1234, OK))

      println {
        res3
      }

      // И в какой-то момент, когда мы захотим использовать другой ShipmentStorage,
      // нам не нужно ничего менять в нашей бизнес-логике (OperationService).
      // Мы просто выполняем тот же код с другим хранилищем в качестве параметра для Kleisli, и это почти все.

      // В целом мы видим, что Kleisli обеспечивает очень функциональный способ внедрения зависимостей в Scala.

    }
  }

  part1.chapter1

}


