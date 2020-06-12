package ArrayAlgorithms

import scala.collection.mutable.ArrayBuffer
import scala.Array
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

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

    /**
      * Leetcode 3
      * There are two sorted arrays nums1 and nums2 of size m and n respectively.
      * Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).
      * You may assume nums1 and nums2 cannot be both empty.
      * @param nums1
      * @param nums2
      * @return
      */
    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
        val array = (nums1 ++ nums2).sorted
        val mednum = array.length / 2
        if (array.length % 2 == 0) return (array(mednum) + array(mednum - 1)) / 2.0
        array(array.length/2)
    }

    /**
      * Leetcode 11 
      * Container With Most Water
      * @param height
      * @return
      */
     def maxArea(height: Array[Int]): Int = {
        var maxArea:Int = 0
        var (l, r) = (0, height.length -1)
        
        while(l < r) {
            val hl = height(l)
            val hr = height(r)
            maxArea = math.max(maxArea, math.min(hl, hr) * (r-l))
            if(hl < hr) {
                l += 1
            } else {
                r -= 1
            }
        }
        
        maxArea
    }

    /**
      * Leetcode 15
      * Given an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0? 
      * Find all unique triplets in the array which gives the sum of zero.
      * @param nums
      * @return
      */
    def threeSum(nums: Array[Int]): List[List[Int]] = {        
        val res = new ListBuffer[List[Int]]()
        val len = nums.length
        if (nums == null || len < 3) return res.toList
        val num_sort = nums.sorted        
        breakable {
            for (i <- 0 until(len - 2)) {
                if (num_sort(i) > 0) break
                breakable {
                    if(i > 0 && num_sort(i) == num_sort(i - 1)) break
                    var L = i + 1
                    var R  = len - 1
                    while (L < R) {
                        val sum = num_sort(i) + num_sort(L) + num_sort(R)
                        if (sum == 0) {
                            res.append(List[Int](num_sort(i), num_sort(L), num_sort(R)))
                            while (L<R && num_sort(L) == num_sort(L + 1)) L += 1; // delete duplicate
                            while (L<R && num_sort(R) == num_sort(R - 1)) R -= 1; // delete duplicate
                            L += 1;
                            R -= 1;
                        } 
                        else if (sum < 0) L += 1
                        else R -= 1
                    }
                }
            }
        }        
        res.toList
    }
    
    /**
      * Leetcode 16
      *
      * @param nums
      * @param target
      * @return
      */
    def threeSumClosest(nums: Array[Int], target: Int): Int = {
        val len = nums.length
        val num_sort = nums.sorted  
        var ans = num_sort(0) + num_sort(1) + num_sort(2)
        for (i <- 0 until(len - 2)) {
            var L = i + 1
            var R = len - 1
            while (L < R) {
                val sum = num_sort(i) + num_sort(L) + num_sort(R)
                if (math.abs(target - sum) < math.abs(target - ans)) ans = sum
                if (sum > target) R -= 1
                else if (sum < target) L += 1
                else return ans
            }
        }
        ans
    }
}
