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

    /**
      * Leetcode 14
      * Write a function to find the longest common prefix string amongst an array of strings.
      * If there is no common prefix, return an empty string ""
      * @param strs
      * @return
      */
    def longestCommonPrefix(strs: Array[String]): String = {
        def longestCommonPrefix(str1: String, str2: String): String = {
            val length = math.min(str1.length(), str2.length())
            var index = 0
            while (index < length && str1(index) == str2(index)) index += 1
            str1.substring(0, index)
        }
        import scala.util.control.Breaks._
        if (strs == null || strs.length == 0) return ""
        var prefix = strs(0)
        var count = strs.length
        breakable {
            for (i <- 1 until count) {
                prefix = longestCommonPrefix(prefix, strs(i))
                if (prefix.length() == 0) break
            }
        }
        prefix
    }

    /**
     * Leetcode 17
     * Letter Combinations of a Phone Number
     * Input: "23"
     * Output: ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].
     */
    def letterCombinations(digits: String): List[String] = {
        val map = scala.collection.immutable.Map(
            2 -> List('a','b','c'),
            3 -> List('d','e','f'),
            4 -> List('g','h','i'),
            5 -> List('j','k','l'),
            6 -> List('m','n','o'),
            7 -> List('p','q','r','s'),
            8 -> List('t','u','v'),
            9 -> List('w','x','y','z')            
        )
        var ans = new scala.collection.mutable.ListBuffer[String]()
        if (digits.isEmpty) Nil
        for (letter <- map(digits(0).asDigit)) ans.append(letter.toString())
        for (i <- 1 until digits.length()) {
            val newAns = new scala.collection.mutable.ListBuffer[String]()
            for (letter <- map(digits(i).asDigit)) {
                for (com <- ans) newAns.append(com + letter)
            }
            ans = newAns
        }
        ans.toList
    }

    /**
     * Leetcode 17
     * Letter Combinations of a Phone Number
     * Recursively
     * Input: "23"
     * Output: ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"].
     */
    def rcLetterCombinations(digits: String): List[String] = {
        def letters(digit: Char): List[Char] = digit match {
            case '2' => List('a', 'b', 'c')
            case '3' => List('d', 'e', 'f')
            case '4' => List('g', 'h', 'i')
            case '5' => List('j', 'k', 'l')
            case '6' => List('m', 'n', 'o')
            case '7' => List('p', 'q', 'r', 's')
            case '8' => List('t' ,'u', 'v')
            case '9' => List('w', 'x', 'y', 'z')
        }

        def comb(digits: List[Char]): Seq[String] = digits match {
            case Nil => Seq("")
            case d :: ds =>
                val cs = comb(ds)
                for {
                    hd <- letters(d)
                    tl <- cs
                } yield hd +: tl
        }
        if (digits.isEmpty) Nil
        else comb(digits.toList).toList
    }

    /**
     * Leetcode 28
     * Implement strStr().
     * Return the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.
     * Using KMP
     * @param haystack
     * @param needle
     * return Int
     */
    def strStr(haystack: String, needle: String): Int = {
        if (needle.isEmpty) return 0
        if (haystack.isEmpty()) return -1
        var (i, j) = (0, -1)
        val kmp = new Array[Int](needle.length() + 1)
        kmp(i) = j
        while (i < needle.length()) {
            while (j >= 0 && needle(i) != needle(j)) j = kmp(j)
            i += 1; j += 1
            kmp(i) = j
        }
        j = 0; i = 0
        while (j < haystack.length()) {
            while (i >=0 && needle(i) != haystack(j)) i = kmp(i)
            i += 1; j += 1
            if (i == needle.length()) return j - needle.length()
        }
        return -1
    }
}
