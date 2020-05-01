package Tree

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}
import scala.collection.mutable.Stack
/**
  * The Algorithms of Binary Tree 
  * 
  */
object BinaryTree {

    /**
     * Leetcode 105
     * Description：Given preorder and inorder traversal of a tree, construct the binary tree
     * You may assume that duplicates do not exist in the tree.
     * For example, given
     * preorder = [3,9,20,15,7]
     * inorder = [9,3,15,20,7]
     * return [3,9,20,null,null,15,7] serialized string of tree
     * @param preorder   - a array of preorder traverse
     * @param inorder   - a array of inorder traverse
     * @return - root TreeNode
     */
    def buildFromPreIn(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
        if (inorder.isEmpty) null
        else {
            val root = new TreeNode(preorder(0))
            val  leftSubTreeNodeNums = inorder.indexOf(root.value)
            root.left = buildFromPreIn(preorder.slice(1, leftSubTreeNodeNums + 1),
                inorder.slice(0, leftSubTreeNodeNums)) 
            root.right = buildFromPreIn(preorder.slice(leftSubTreeNodeNums+1,preorder.length),
                inorder.slice(leftSubTreeNodeNums+1,inorder.length))          
            root
        }       
    }
    /** 
      * Leetcode 106
      * Discription: Given inorder and postorder traversal of a tree, construct the binary tree.
      * inorder = [9,3,15,20,7]
      * postorder = [9,15,7,20,3]
      * return [3,9,20,null,null,15,7] level serialized string of tree
      * @param inorder      - a array of inorder traverse
      * @param postorder    - a array of postorder traverse
      * @return root Treenode
      */
    def buildFromInPost(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
        if (inorder.isEmpty) null
        else {
            val root = new TreeNode(postorder.last)
            val rightSubTreeNodeNums = inorder.indexOf(root.value)
            root.right = buildFromInPost(inorder.slice(rightSubTreeNodeNums+1, inorder.length),
                postorder.slice(rightSubTreeNodeNums, postorder.length - 1))
            root.left = buildFromInPost(inorder.slice(0, rightSubTreeNodeNums), 
                postorder.slice(0, rightSubTreeNodeNums))       
            root
        }   
    }
    
    /**
      * Leetcode 101
      * Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).
      * recursively solution
      * @param root
      * @return
      */
    def rcSymmetric(root: TreeNode): Boolean = {
        if (root == null) return true
        def isMirror(t1: TreeNode,t2: TreeNode): Boolean = {
            if (t1 == null && t2 == null) return true
            if (t1 == null || t2 == null) return false
            (t1.value == t2.value) && isMirror(t1.right, t2.left) && isMirror(t1.left, t2.right)
        }
        isMirror(root.left,root.right)
    }
    /**
      * Leetcode 101
      * Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).
      * iteratively solution
      * @param root
      * @return
      */
    def isSymmetric(root: TreeNode): Boolean = {
        val queue = new Queue[TreeNode]()
        queue.enqueue(root)
        queue.enqueue(root)
        while (queue.nonEmpty) {
            val t1 = queue.dequeue
            val t2 = queue.dequeue
            if (t1 == null && t2 == null) { Unit }
            else {
                if (t1 == null || t2 == null) return false
                if (t1.value != t2.value) return false
                queue.enqueue(t1.left)
                queue.enqueue(t2.right)
                queue.enqueue(t1.right)
                queue.enqueue(t2.left)
            }          
        }
        true
    }

    /** 
      * Leetcode 102
      * Discription:Given a binary tree, return the level order traversal of its nodes' values.
      * (ie, from left to right, level by level).
      * return its level order traversal
      * @param root     - root treenode 
      * @return         - traversal of its nodes' values.
      */
    def levelOrder(root: TreeNode): List[List[Int]] = {
        val list = new ListBuffer[List[Int]]()
        if (root == null) {
            list.toList 
        } else {
            val queue = new Queue[TreeNode]
            queue.enqueue(root)
            while (queue.nonEmpty) {
                val rowList = (1 to queue.size).foldLeft(List[Int]()) { (str, _) =>
                    val temp = queue.dequeue
                    if (temp.left != null) queue.enqueue(temp.left)
                    if (temp.right != null) queue.enqueue(temp.right)
                    str :+ temp.value
                }
                list.append(rowList)
            }
        }        
        list.toList
    }

    /**
      * Leetcode 103
      * Given a binary tree, return the zigzag level order traversal of its nodes' values. 
      * (ie, from left to right, then right to left for the next level and alternate between).
      * Given binary tree [3,9,20,null,null,15,7]
      * return [[3],[20,9],[15,7]]
      * @param root     - root Treenode
      * @return         - zigzag level order
      */
    def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
        val list = new ListBuffer[List[Int]]()
        if (root == null) {
            list.toList 
        } else {
            var flag = false
            val queue = new Queue[TreeNode]
            queue.enqueue(root)
            while (queue.nonEmpty) {
                val rowList = (1 to queue.size).foldLeft(List[Int]()) { (str, _) =>
                    val temp = queue.dequeue
                    if (temp.left != null) queue.enqueue(temp.left)
                    if (temp.right != null) queue.enqueue(temp.right)
                    str :+ temp.value
                }
                if (flag) list.append(rowList.reverse) else list.append(rowList)
                flag = !flag
            }
        }
        list.toList       
    }

    /** 
      * Leetcode 107
      * Discription:Given a binary tree, return the bottom-up level order traversal of its nodes' values. 
      * (ie, from left to right, level by level from leaf to root).
      * return its level order bottom up traversal
      * @param root     - root treenode 
      * @return         - traversal of its nodes' values.
      */
    def levelOrderBottom(root: TreeNode): List[List[Int]] = {
        val list = new ListBuffer[List[Int]]()
        if (root == null) {
            list.toList 
        } else {
            val queue = new Queue[TreeNode]
            queue.enqueue(root)
            while (queue.nonEmpty) {
                val rowList = (1 to queue.size).foldLeft(List[Int]()) { (str, _) =>
                    val temp = queue.dequeue
                    if (temp.left != null) queue.enqueue(temp.left)
                    if (temp.right != null) queue.enqueue(temp.right)
                    str :+ temp.value
                }
                list.append(rowList)
            }
        }        
        list.toList.reverse
    }
    /**
      * Leetcode 94
      * Given a binary tree, return the inorder traversal of its nodes' values.
      * recrusively solution
      * @param root
      * @return
      */
    def rcInorderTraversal(root: TreeNode): List[Int] = {
        val list = ListBuffer[Int]()
        def inorder(root:TreeNode,list: ListBuffer[Int]): Unit={
            if (root != null){
                if (root.left != null) inorder(root.left, list)
                list.append(root.value)
                if (root.right != null) inorder(root.right, list)
            }  
        } 
        inorder(root, list)
        list.toList
    }

    /**
      * Leetcode 94
      * Given a binary tree, return the inorder traversal of its nodes' values.
      * iteratively solution
      * @param root
      * @return
      */
    def inorderTraversal(root: TreeNode): List[Int] = {
        var stack = List[TreeNode]()
        val list = ListBuffer[Int]()
        var curNode = root
        while (curNode != null || stack.nonEmpty){
            while (curNode != null) {
                stack = curNode :: stack
                curNode = curNode.left
            }
            curNode = stack.head
            stack = stack.tail
            list.append(curNode.value)
            curNode = curNode.right
        }
        list.toList
    }

    /** 
      * Leetcode 104
      * Discription:Given a binary tree, find its maximum depth. 
      * The maximum depth is the number of nodes along the longest path from the root node down to the farthest leaf node.
      * return maximum depth
      * using recrusive
      * @param root     - root treenode 
      * @return         - Int
      */
    def rcmaxDepth(root: TreeNode): Int = {
        if (root == null) return 0
        math.max(rcmaxDepth(root.left), rcmaxDepth(root.right)) + 1 
    }

    /** 
      * Leetcode 104
      * Discription:Given a binary tree, find its maximum depth. 
      * The maximum depth is the number of nodes along the longest path from the root node down to the farthest leaf node.
      * return maximum depth
      * @param root     - root treenode 
      * @return         - Int
      */
    def maxDepth(root: TreeNode): Int = {
        if (root == null) return 0
        val queue = new Queue[TreeNode]
        var depth = 0
        queue.enqueue(root)
        while (queue.nonEmpty){
            depth += 1
            (1 to queue.size).foreach(x=>{
                val curNode = queue.dequeue
                if (curNode.left != null) queue.enqueue(curNode.left)
                if (curNode.right != null) queue.enqueue(curNode.right) 
            })
        }
        depth
    }
    /**
      * Leetcode 111
      * Given a binary tree, find its minimum depth.
      * @param root
      * @return
      */
    def rcMinDepth(root: TreeNode): Int = {
        def helper(root:TreeNode): Int={
            if (root == null) return 0
            if (root.left == null && root.right == null) return 1
            if (root.left == null) return helper(root.right) + 1 
            if (root.right == null) return helper(root.left) + 1 
            return math.min(helper(root.left), helper(root.left)) + 1           
        }
        helper(root)
    }

    /**
     * Leetcode 297
     * Preorder recrusive serialize a binary tree from a root tree node
     * @param root  - a root Treenode
     * @return - serialized string of tree
     */
    def rcserialize(root: TreeNode): String = {
        if (root == null) {
            return "null"
        } 
        return root.value.toString + "," + rcserialize(root.left) + "," + rcserialize(root.right)
    }

    /** 
      * Leetcode 297
      * Description：Level order traversal serialize
      * preorder as Array(1,2,3,4,5) and inorder as Array(2,1,4,3,5)
      * from preorder and inorder build a binary tree,serialize this tree
      * return [1,2,3,null,null,4,5]
      * @param root - a root Treenode
      * @return - serialized string of tree
      */
    def serialize(root: TreeNode): String = {
        val queue = new Queue[TreeNode]
        val str = ArrayBuffer.empty[String]
        queue.enqueue(root)
        while (!queue.isEmpty){
            val curNode = queue.dequeue
            if (curNode != null){                
                str += curNode.value.toString
                queue.enqueue(curNode.left)
                queue.enqueue(curNode.right)
            } else {
                str += null
            }
        }
        // remove tail "null" nodes
        str.reverse.dropWhile( {x:String => x == null} ).reverse.mkString("[", ",", "]")
    }
    /**
      * Leetcode 110
      * Given a binary tree, determine if it is height-balanced.
      * @param root - root Treenode
      * @return boolean 
      */
    def isBalanced(root: TreeNode): Boolean = {
        def helper(root: TreeNode): Int = {
            if (root == null) return 0
            val left = helper(root.left)
            val right = helper(root.right)
            if (left == -1 || right == -1 || math.abs(left - right)>1) return -1
            return math.max(left,right) + 1
        }
        return helper(root) >= 0
    }
}
