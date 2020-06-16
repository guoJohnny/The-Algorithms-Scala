package StringAlgorithms
import org.scalatest.FlatSpec

class StringAlgorithmsSpec extends FlatSpec {
    
    "Longest Substring Without Repeating Characters" should "return a number passed to it" in {  
      assert(StringAlgorithms.lengthOfLongestSubstring("abcabcbb") === 3)
    }

    "Longest Palindromic Substring" should "return a string passed to it" in {  
      assert(StringAlgorithms.longestPalindrome("babad") === "bab")
    }
}
