package StringAlgorithms
import org.scalatest.FlatSpec

class StringAlgorithmsSpec extends FlatSpec {
    
    "Longest Substring Without Repeating Characters" should "return a number passed to it" in {  
      assert(StringAlgorithms.lengthOfLongestSubstring("abcabcbb") === 3)
    }
}
