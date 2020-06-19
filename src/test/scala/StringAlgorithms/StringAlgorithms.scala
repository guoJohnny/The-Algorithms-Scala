package StringAlgorithms
import org.scalatest.FlatSpec

class StringAlgorithmsSpec extends FlatSpec {
    
    "Longest Substring Without Repeating Characters" should "return a number passed to it" in {  
      assert(StringAlgorithms.lengthOfLongestSubstring("abcabcbb") === 3)
    }

    "Longest Palindromic Substring" should "return a string passed to it" in {  
      assert(StringAlgorithms.longestPalindrome("babad") === "bab")
      assert(StringAlgorithms.longestPalindromeCentralDiffusion("babad") === "aba" || 
            StringAlgorithms.longestPalindromeCentralDiffusion("babad") === "bab")
    }

    "String to Integer (atoi)" should "return a number passed to it" in {  
      assert(StringAlgorithms.myAtoi("  -42") === -42)
      assert(StringAlgorithms.myAtoi("42") === 42) 
    }
}
