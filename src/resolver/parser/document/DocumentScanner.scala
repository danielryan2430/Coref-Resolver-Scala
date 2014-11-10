package resolver.parser.document
import scala.io.Source

/**
 * Created by dimberman on 11/9/14.
 */
class DocumentScanner {

  def scanDocument(fileName:String): Document= {
    val srce = Source.fromFile(fileName)
    val s = srce.mkString
    srce.close()
    val s1 = scanChar(s, "", "", 0, Seq());
    return new Document(s1)
  }

  def scanChar(s: String, prevWord: String, currentWord: String, sentenceNum: Int, c: Seq[FeatureSet]): Seq[FeatureSet] = {
    s.head match {
      case '<' => pullIdent(s.tail, "", prevWord, sentenceNum, c)
      case ' ' => scanChar(s.tail, currentWord, "", sentenceNum, c)
      case '\0' => c
      case '.' => scanChar(s.tail, prevWord, currentWord :+ s.head, sentenceNum + 1, c)
      case _ => scanChar(s.tail, prevWord, currentWord :+ s.head, sentenceNum, c)
    }
  }


  def pullIdent(s: String, ident: String, prevWord: String, sentenceNum: Int, c: Seq[FeatureSet]): Seq[FeatureSet] = {
    val currentLetter = ident.head
    currentLetter match {
      case '>' => {
        if (ident.contains("/COREF")) {
          val nextWord = peekAhead(s.tail,"")
          scanChar(s.tail, c.last.lastWord, "", sentenceNum, c :+ identifyFeatureSet(ident :+ currentLetter, c, sentenceNum, prevWord, nextWord))
        }
        pullIdent(s.tail, ident :+ currentLetter, prevWord, sentenceNum, c)
      }
      case '.' => pullIdent(s.tail, ident :+ currentLetter, prevWord, sentenceNum + 1, c)
      case _ => pullIdent(s.tail, ident :+ currentLetter, prevWord, sentenceNum, c)
    }
  }


  def identifyFeatureSet(s: String, c: Seq[FeatureSet], sentenceNum: Int, prevWord: String, nextWord: String): FeatureSet = {
    //    val feature =  pullFeatures(s, null,null,null,null,null,null,null,null,null, c, sentenceNum)
    val splitCriteria = "<|>"
    val splitString = s.split(splitCriteria)
    val refID = findRefID(splitString(0))
    val sDist = findSentenceDistance(refID, c, sentenceNum)
    val mDist = findMentionDistance(refID, c, 0)
    val mentionType = findType(splitString(0))
    val completePhrase = splitString(1)
    val semHead = findSemHead(splitString(1))
    val firstWord = findFirstWord(splitString(1))
    val lastWord = findLastWord(splitString(1))


    new FeatureSet(refID, sentenceNum, mentionType, completePhrase, semHead, firstWord, lastWord, prevWord, nextWord, sDist, mDist)

  }

  //  def pullFeatures(s:String, mType:String, cString:String, sHead:String, fWord:String, lWord:String, pWord:String, nWord:String, sDist:Int, mDist:Int, c: Seq[Classifier], sentenceNum:Int): Classifier ={
  //
  //  val refID:Int = null
  //
  //
  //  }


  val itemFinder: (String, String, String, (String, String) => String) => String =
    (s: String, current: String, mtch: String, func: (String, String) => String) => {
      current match {
        case mtch => func(s.tail, "")
        case _ => itemFinder(s.tail, current :+ s.head, mtch, func)
      }
    }


  def findRefID(s: String): Int = {
    itemFinder(s, "", "ID=\"", pullNumberString).toInt
  }

  def pullNumberString(s: String, current: String): String = {
    s.head match {
      case "\"" => current
      case _ => pullNumberString(s.tail, current :+ s.head)
    }
  }


  def findType(s: String): String = {
    itemFinder(s, "", "TYPE=\"", pullType)
  }

  def pullType(s: String, current: String): String = {
    s.head match {
      case '\"' => current
      case _ => pullType(s.tail, current :+ s.head)
    }
  }


  def findSemHead(s: String): String = {
    //I have no idea how to do this
    "Agsad"
  }

  def findFirstWord(s: String): String = {
    s.split(" ").head
  }

  def findLastWord(s: String): String = {
    s.split(" ").last
  }

  def findSentenceDistance(refID: Int, c: Seq[FeatureSet], currentSentence: Int): Int = {
    c.last match {
      case null => -1
      case s if c.last.refID == refID => currentSentence - c.last.sentenceNum
      case _ => findSentenceDistance(refID, c.init, currentSentence)
    }
  }


  def findMentionDistance(refID: Int, c: Seq[FeatureSet], distance: Int): Int = {
    c.last match {
      case null => -1
      case s if c.last.refID == refID => distance
      case _ => findSentenceDistance(refID, c.init, distance + 1)
    }
  }

  def peekAhead(s: String, current:String):String = {
       s.head match {
         case ' ' =>{
           if(current=="") peekAhead(s.tail, current)
           else current
         }
         case '<' => pushPastAnnotation(s)
         case _ => peekAhead(s.tail, current:+s.head)
       }
  }

  def pushPastAnnotation(s:String): String = {
         s.head match {
           case '>' => peekAhead(s.tail, "")
           case _ => pushPastAnnotation(s.tail)
         }
  }


}


class FeatureSet(rID: Int, sNum: Int, mType: String, cString: String, sHead: String, fWord: String, lWord: String, pWord: String, nWord: String, sDist: Int, mDist: Int) {
  val refID: Int = rID
  val sentenceNum: Int = sNum
  val mentionType: String = mType
  val completeString: String = cString
  val semanticHead: String = sHead
  val firstWord: String = fWord
  val lastWord: String = lWord
  val previousWord: String = pWord
  val nextWord: String = nWord
  val sentenceDistance: Int = sDist
  val mentionDistance: Int = mDist
}
