package classifier
import resolver.parser.document._
/**
 * Created by serenity on 11/10/14.
 */
object bayesianClassifier {
  def classify(weights: Seq[Double], d: Document, featureExtractor: (Document, Int, Int) => Seq[Double]) = {
    def weightMult: Seq[Double] => Double =
      (input: Seq[Double]) => input.zip(weights).foldLeft(0.0) { case (a, (b, c)) => a + b * c};

    ((List[Int](), List[FeatureSet]()) /: d.features) {
      //Part one of the accu  the second part of the accumulator holds all of the references seen so far so we can build the mention list up correctly.
      (accum, liveFeature) => (accum._1 :+ ((liveFeature.refID, //the returned feature has a base setting of new (score btw f.ID and itself...)
        weightMult(featureExtractor(d, liveFeature.refID, liveFeature.refID))) /: accum._2) { (maxChoiceAccum, maxChoiceFeat) => {
        //this function takes the max choice for a feature comp...
        val C = weightMult(featureExtractor(d, liveFeature.refID, maxChoiceFeat.refID))
        if (C > maxChoiceAccum._2) {
          (maxChoiceFeat.refID, C)
        } // if You have a higher score, choose you
        else {
          maxChoiceAccum
        } //otherwise keep the old score
      }
      }._1 // then return the choice Made.
        , accum._2 :+ liveFeature) // bind to it the list of all choices to consider next time
    }._1 //return the list of assigned mentions--it is ordered so it directly corresponds to an A vector.
  } // finish up.
}
