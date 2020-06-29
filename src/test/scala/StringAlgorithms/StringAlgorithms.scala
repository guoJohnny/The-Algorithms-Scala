package StringAlgorithms
import org.scalatest.FlatSpec
import scala.Array

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

    "String common prefix" should "return a common prefix passed to it" in {  
      assert(StringAlgorithms.longestCommonPrefix(Array("flower","flow","flight")) === "fl")
      assert(StringAlgorithms.longestCommonPrefix(Array("dog","racecar","car")) === "") 
    }

    "Letter Combinations" should "return a List passed to it" in {  
      val list = List("ad", "bd", "cd", "ae", "be", "ce", "af", "bf", "cf")
      assert(StringAlgorithms.letterCombinations("23").diff(list) === List())
      assert(StringAlgorithms.rcLetterCombinations("23").diff(list) === List())
    }

    "Implement strStr()" should "return a int passed to it" in {  
      assert(StringAlgorithms.strStr("hello", "ll") === 2) 
    }

    "Multiply Strings" should "return a string passed to it" in {  
      assert(StringAlgorithms.multiply("123", "456") === "56088") 
    }

    "CountAndSay" should "return a string passed to it" in {  
      assert(StringAlgorithms.countAndSay(4) === "1211") 
    }
}
