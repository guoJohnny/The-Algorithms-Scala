package ArrayAlgorithms

import scala.collection.mutable.ArrayBuffer

object ArrayAlgorithms {
    
    /**
      * Remove all negative numbers in arraybuffer except the first negative number
      *
      * @param array    - Arraybuffer
      */
    def removeNegativeNumber(array: ArrayBuffer[Int]) : Unit = {
        var first = true
        val index = for(i <- 0 until array.length if first || array(i) > 0) yield {
            if (array(i) < 0) first = false ; i
        }
        for (i <- 0 until index.length) array(i) = array(index(i))  
        array.trimEnd(array.length - index.length) 
    }
}
