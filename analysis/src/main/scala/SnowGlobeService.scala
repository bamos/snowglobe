package io.bamos.snowglobe

// Akka
import akka.actor.{Actor,ActorRefFactory}
import akka.pattern.ask
import akka.util.Timeout

// Spray
import spray.http._
import spray.routing.HttpService
import MediaTypes._

// Scala
import scala.concurrent.duration._

// Actor accepting Http requests for the Scala collector.
class SnowGlobeServiceActor extends Actor with HttpService {
  implicit val timeout: Timeout = 1.second // For the actor 'asks'
  def actorRefFactory = context

  // Use SnowGlobeService so the same route can be accessed differently
  // in the testing framework.
  private val snowGlobeService = new SnowGlobeService(context)

  // Message loop for the Spray service.
  def receive = handleTimeouts orElse runRoute(snowGlobeService.collectorRoute)

  def handleTimeouts: Receive = {
    case Timedout(_) => sender !
      HttpResponse(status = 404, entity = "404 Not found")
  }
}

class SnowGlobeService(context: ActorRefFactory) extends HttpService {
  implicit def actorRefFactory = context
  val collectorRoute = {
    get {
      pathSingleSlash {
        redirect("/index", StatusCodes.Found)
      }~
      path("index") {
        respondWithMediaType(`text/html`) {
          complete{
            html.index().toString
          }
        }
      }~
      path("favicon.ico") {
        complete(StatusCodes.NotFound)
      }~
      path(Rest) { path =>
        getFromResource("bootstrap/%s" format path)
      }~
      path("visitors") {
        parameters('csvCols ? "dvce_tstamp,page_urlpath") { (csvCols) =>
          respondWithMediaType(`text/html`) {
            complete(
              QueryHandler.visitors(csvCols.split(","))
            )
          }
        }
      }~
      path("headers") {
        respondWithMediaType(`text/html`) {
          complete(
            html.headers(Helper.headers).toString
          )
        }
      }
    }~
    complete(HttpResponse(status = 404, entity = "404 Not found"))
  }
}
