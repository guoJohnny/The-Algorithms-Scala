package ArrayAlgorithms

import scala.Array
import org.scalatest.FlatSpec
import scala.collection.mutable.ArrayBuffer

class ArrayAlgorithmsSpec extends FlatSpec{
    
    "ArrayBuffer remove negative number" should "return a removed arraybuffer passed to it" in {     
        val arraybuffer = Array(2, 3, 11, -1, -2, -3, 22, 21).to[ArrayBuffer]
        ArrayAlgorithms.removeNegativeNumber(arraybuffer)
        assert(arraybuffer.toArray === Array(2, 3, 11, -1, 22, 21))
    }

}
