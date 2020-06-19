package StringAlgorithms
import scala.Array

object StringAlgorithms {
    /**
      * Leetcode 3
      * Given a string, find the length of the longest substring without repeating characters.
      * @param s
      * @return
      */
    def lengthOfLongestSubstring(s: String): Int = {
        import scala.collection.mutable.{Map}
        if (s == null || s.length() == 0) return 0
        var max = 0
        val map:Map[Char, Int] = Map()
        var start = 0
        for (i <- 0 until(s.length())) {
            var cur = s.charAt(i)
            val index = map.getOrElse(cur, -1)
            if (start <= index) {
                val curMax = i - start
                max = if (curMax > max) curMax else max
                start = index + 1
            }
            map.put(cur, i)
        }
        max = if (max > (s.length() - start)) max else s.length() - start
        return max
    }

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

    /**
      * Leetcode 5
      * Given a string s, find the longest palindromic substring in s. 
      * You may assume that the maximum length of s is 1000.
      * @param s
      * @return
      */
    def longestPalindromeCentralDiffusion(s: String): String = {
        def expandAroundCenter(s: String, left: Int, right: Int): Int = {
            var L = left; var R = right
            while (L >= 0 && R < s.length() && s.charAt(L) == s.charAt(R)) { 
                L -= 1
                R += 1
            } 
            return R - L - 1;
        }
        if (s == null || s.length() < 1) return ""
        var (start, end) = (0, 0)
        for (i <- 0 until s.length()) {
            val len1 = expandAroundCenter(s, i , i)
            val len2 = expandAroundCenter(s, i , i + 1)
            val len = math.max(len1, len2)
            if (len > end - start) {
                start = i - (len - 1) / 2
                end = i + len / 2
            } 
        }
        s.substring(start, end + 1)
    }

    /**
      * Leetcode 8
      * atoi
      * @param str
      * @return
      */
    def myAtoi(str: String): Int = {
        val str_tmp = str.trim()
        if (str_tmp == null || str_tmp.length() < 1 ) return 0
        var index = 0
        var negative = false
        var ans = 0
        if (str_tmp(index) == '-') {
            index += 1
            negative = true
        }else if (str_tmp(index) == '+' ) index += 1
        else if (!Character.isDigit(str_tmp(index))) return 0
        
        while (index < str_tmp.length() && Character.isDigit(str_tmp(index))) {
            val digit = str_tmp(index) - '0'
            if (ans > (Int.MaxValue - digit) / 10) {
                return if (negative) Int.MinValue else Int.MaxValue
            }
            ans = ans * 10 + digit
            index += 1
        }        
        return if (negative) -ans else ans
    }
}
