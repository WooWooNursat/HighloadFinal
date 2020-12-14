import akka.Done
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.Source
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

class CalculatorService (implicit system: ActorSystem[_], ex: ExecutionContextExecutor) {

  private val config = system.settings.config.getConfig("akka.kafka.producer")
  private val server = system.settings.config.getString("akka.kafka.producer.kafka-clients.server")
  private val topic  = system.settings.config.getString("akka.kafka.producer.kafka-clients.topic1")

  private val producerSettings =
    ProducerSettings(config, new StringSerializer, new StringSerializer)
      .withBootstrapServers(server)

  def send(token: String, expression: String, response: String): Future[Done] = {
    Source(Seq[String](token, expression, response))
      .map(value => new ProducerRecord[String, String](topic, value))
      .runWith(Producer.plainSink(producerSettings))
  }
}
