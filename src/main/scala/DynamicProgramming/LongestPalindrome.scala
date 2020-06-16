package DynamicProgramming
import scala.Array
object LongestPalindrome {
    
    /**
      * Leetcode 5
      * Given a string s, find the longest palindromic substring in s. 
      * You may assume that the maximum length of s is 1000.
      * @param s
      * @return
      */
    def longestPalindrome(s: String): String = {
        val len = s.length()
        if (len < 2) return s
        var maxLen = 1
        var begin = 0
        var dp = Array.ofDim[Boolean](len, len)   
        // init 
        for (i <- 0 until len) {
            dp(i)(i) = true
        }
        // dp
        for (j <- 1 until len) {
            for (i <- 0 until j) {
                if (s(i) != s(j)) dp(i)(j) = false 
                else {
                    if (j - i < 3) dp(i)(j) = true
                    else dp(i)(j) = dp(i+1)(j-1)
                }
                if (dp(i)(j) && j - i + 1 > maxLen) {
                    maxLen = j - i + 1
                    begin = i
                }
            }
        }
        s.substring(begin, begin + maxLen)
    }
}
