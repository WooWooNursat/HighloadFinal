import java.util.Timer

import akka.Done
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.DateTime
import akka.kafka.scaladsl.Consumer
import akka.kafka.scaladsl.Consumer.DrainingControl
import akka.kafka.{ConsumerSettings, Subscriptions}
import akka.stream.scaladsl.{Keep, Sink}
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
import org.apache.kafka.common.TopicPartition
import org.apache.kafka.common.serialization.StringDeserializer

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

final case class History(history: Seq[Calc])
final case class Calc(request: String, response: String)

class HistoryService(implicit system: ActorSystem[_], ex: ExecutionContextExecutor) {

//  implicit val system = ActorSystem[Nothing](Behaviors.empty, "Consumer")
//  implicit val ex = system.executionContext

  private val topic = system.settings.config.getString("akka.kafka.producer.kafka-clients.topic1")
  private val configCon = system.settings.config.getConfig("akka.kafka.consumer")
  private val consumerSettings = ConsumerSettings(configCon, new StringDeserializer, new StringDeserializer)
    .withBootstrapServers("localhost:9092")
    .withGroupId("HistoryService")
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")

  private var history = Seq.empty[Calc]

  private var consumer:DrainingControl[Done] = null

  def done(): History = {
    consumer.stop()
    History(history.filterNot(_ == Calc(null, null)))
  }

  def control(token: String): Unit = {
    history = Seq.empty
    consumer =
      Consumer
      .plainSource(
        consumerSettings,
        Subscriptions.assignmentWithOffset(
          new TopicPartition(topic, 0) -> 0.toLong
        )
      )
      .grouped(3)
      .map { records =>
        if (token.equals(records(0).value)) {
          println(records(0).value,records(1).value, records(2).value)
          history = history :+ Calc(records(1).value, records(2).value)
        }
        else Calc(null, null)
      }
      .toMat(Sink.ignore)(DrainingControl.apply)
      .run()
  }

//  def runRequest(records: Seq[ConsumerRecord[String, String]]): Future[Calc] = {
//    println(records(1).value, records(2).value)
//    history = history :+ Calc(records(1).value, records(2).value)
//    Future.successful(Calc(records(1).value, records(2).value))
//  }
}