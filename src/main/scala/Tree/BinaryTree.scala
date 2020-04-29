package Tree

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

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
     * 
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

}
