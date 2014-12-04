package classifier

import resolver.parser.document._
import scala.util.Random
/**
 * Created by serenity on 12/3/14.
 */
object adaGradTrainer {
  def train(documents:Seq[Document], eta:Double, lambda:Double, iterMax:Int, featureCount:Int, featureExtractor: (Document, Int, Int) => Seq[Double]):Seq[Double] = {
    var grad =  List.fill(featureCount)(0.0)
    var dgrad = List.fill(featureCount)(0.0)
    var weights =     List.fill(featureCount)(0.0)
    var error=0.0
    var iter = 0
    while( iter<iterMax & error>.0001) {
      grad =  List.fill(featureCount)(0.0)
      dgrad = List.fill(featureCount)(0.0)
      error=0.0
      for (p <- Random.shuffle(documents)) {

        grad = compGrad(p)
        dgrad = dgrad.zip(grad).map { case ((dgtii: Double, gti: Double)) => dgtii + gti * gti}
        val etaOverHtii = dgrad.map(dgtii => eta/(Math.sqrt(dgtii)+1))
        weights =( List[Double]() /: weights.zip(grad).zip(etaOverHtii)) { case (accm, ((xti:Double,gti:Double),eohtii:Double)) => {val inter= xti +gti*eohtii
            accm :+ Math.signum(inter)*(Math.abs(inter) - lambda*eohtii)}
        }
      }
      iter+=1
    }

    weights.toList
  }
}
