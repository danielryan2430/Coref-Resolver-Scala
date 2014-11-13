package classifier

import resolver.parser.document._

/**
 * Created by serenity on 11/10/14.
 */
object bayesianClassifier {

  def classify(weights: Seq[Double], d: Document, featureExtractor: (Document, Int, Int) => Seq[Double]): List[Int] = {

    //using weight vector, create a score for a featureSet
    def weightMult: Seq[Double] => Double = (input: Seq[Double]) => input.zip(weights).foldLeft(0.0) { case (a, (b, c)) => a + b * c};

    def findBestCorrelatedFeature(maxChoiceAccum: (Int, Double), maxChoiceFeat: FeatureSet, liveFeature: FeatureSet): (Int, Double) = {
      //1. the returned feature has a base setting of new (score btw f.ID and itself...)
      val C = weightMult(featureExtractor(d, liveFeature.mentionID, maxChoiceFeat.mentionID))
      if (C > maxChoiceAccum._2) {
        (maxChoiceFeat.mentionID, C)
      } // if You have a higher score, choose you
      else {
        maxChoiceAccum
      } //other
    }




    //D:(List[Int] = mentionIDs(A), List[FeatureSet] = previous mention features), foldLeft all feature descriptions in Document D
    ((List[Int](), List[FeatureSet]()) /: d.features) {
      //Part one of the accum  the second part of the accumulator holds all of the references seen so far so we can build the mention list up correctly.
      //D: accum is the current list of tuples(refID, FeatureSet) while liveFeature represents a current mention that is being used to generate a new tuple value
      (accum, liveFeature) => (accum._1 :+ ((liveFeature.mentionID, /*1*/ weightMult(featureExtractor(d, liveFeature.mentionID, liveFeature.mentionID))) /: accum._2)
            {(maxChoiceAccum, maxChoiceFeat) => findBestCorrelatedFeature(maxChoiceAccum, maxChoiceFeat, liveFeature)}._1 // then return the choice Made.
      , accum._2 :+ liveFeature) // bind to it the list of all choices to consider next time
    }._1 //return the list of assigned mentions--it is ordered so it directly corresponds to an A vector.
  } // finish up.


}
