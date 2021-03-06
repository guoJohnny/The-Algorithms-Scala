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
      * Leetcode 96
      * Given n, how many structurally unique BST's (binary search trees) that store values 1 ... n?
      * Using Catalan number
      * @param n    - number of tree node
      * @return     - unique BST's number
      */
    def numTrees(n: Int): Int = {
        val num = (0 to n-1).foldLeft(1L) {(c, i)=> c * 2 * (2 * i + 1) / (i + 2)}
        num.toInt
    }
    /**
      * Leetcode 95
      * Given an integer n, generate all structurally unique BST's (binary search trees) that store values 1 ... n.
      * example input 3
      * output [[1,null,2,null,3],[1,null,3,2],[2,1,3],[3,1,null,null,2],[3,2,null,1]]
      * @param n    - number of treenode
      * @return     - list of Treenode
      */
    def generateTrees(n: Int): List[TreeNode] = n match {
        case 0 => Nil
        case _ => 
            def generate (start: Int, end: Int): List[TreeNode] = {
                val list = new ListBuffer[TreeNode]()
                if (start > end) {
                    list.append(null) 
                    return list.toList
                }
                if (start == end) {
                    list.append(new TreeNode(start))
                    return list.toList
                }
                start.to(end).foreach(idx =>{
                    for {
                        left <- generate(start, idx - 1)
                        right <- generate(idx + 1, end)                 
                    } list.append({
                        val ret = new TreeNode(idx)
                        ret.left = left
                        ret.right = right
                        ret
                    })
                })
                list.toList
            }
        generate(1, n)
    } 

    /**
      * Leetcode 98
      * Given a binary tree, determine if it is a valid binary search tree (BST).
      * Recursive
      * @param root
      * @return
      */
    def isValidBST(root: TreeNode): Boolean = {
        def helper(root: TreeNode, lower: Long = Long.MinValue, upper: Long = Long.MaxValue): Boolean = {
            if (root == null) return true
            if (root.value <= lower || root.value >= upper) return false        
            return helper(root.right, root.value, upper) && helper(root.left, lower, root.value)
        }
        helper(root)
    }

    /**
      * Leetcode 98
      * Given a binary tree, determine if it is a valid binary search tree (BST).
      * Inorder
      * @param root
      * @return
      */
    def inorderValidBST(root: TreeNode): Boolean = {
        var stack = List[TreeNode]()
        var inorder = Long.MinValue
        var curNode = root
        while (curNode != null || stack.nonEmpty){
            while (curNode != null) {
                stack = curNode :: stack
                curNode = curNode.left
            }
            curNode = stack.head
            stack = stack.tail
            if (curNode.value <= inorder) return false
            inorder = curNode.value
            curNode = curNode.right           
        }
        true
    }

    /** 
      * Leetcode 99 
      * Two elements of a binary search tree (BST) are swapped by mistake.
      * Recover the tree without changing its structure.
      * @param root - a recovered BST
      */
    def recoverTree(root: TreeNode): Unit = {
        var stack = List[TreeNode]()
        var x, y, pred: TreeNode = null
        var curNode = root
        while (curNode != null || stack.nonEmpty){
            while (curNode != null) {
                stack = curNode :: stack
                curNode = curNode.left
            }
            curNode = stack.head
            stack = stack.tail
            if (pred != null && curNode.value < pred.value) {
                if (x == null) {
                    x = pred
                    y = curNode
                } else {
                    y = curNode
                }
            }
            pred = curNode
            curNode = curNode.right       
        }
        // swap node
        x.value = x.value ^ y.value
        y.value = x.value ^ y.value
        x.value = x.value ^ y.value
    }

    /** 
      * Leetcode 99 
      * Two elements of a binary search tree (BST) are swapped by mistake.
      * Recover the tree without changing its structure.
      * Recrusively
      * @param root - a recovered BST
      */
    def rcRecoverTree(root: TreeNode): Unit = {
        var first, second, pred: TreeNode = null
        def helper(root: TreeNode): Unit = {
            if (root == null) return
            helper(root.left)
            if (pred != null && root.value < pred.value) {
                if (first == null) first = pred
                second = root
            }
            pred = root
            helper(root.right)
        }
        helper(root)
        val tmp = first.value
        first.value = second.value
        second.value = tmp
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
      * LeetCode 108
      * Given an array where elements are sorted in ascending order, convert it to a height balanced BST.
      * For this problem, a height-balanced binary tree is defined as a binary tree 
      * in which the depth of the two subtrees of every node never differ by more than 1.
      * @param nums
      * @return
      */
    def sortedArrayToBST(nums: Array[Int]): TreeNode = {
        def helper (left: Int, right: Int): TreeNode = {
            if (left > right) return null
            val p: Int = (left + right) / 2
            val root = new TreeNode(nums(p))
            root.left = helper(left, p - 1)
            root.right = helper(p + 1, right)
            root
        }
        helper(0, nums.length - 1)
    }

    /**
      * Leetcode 109
      * Given a singly linked list where elements are sorted in ascending order, 
      * convert it to a height balanced BST.
      * @return
      */
    def sortedListToBST(head: ListNode): TreeNode = {
        def listToArray(list: ListNode): Array[Int] = {
            val array = new ArrayBuffer[Int]()
            var head = list
            if (head == null) return array.toArray
            while(head != null) {
                array.append(head.x)
                head = head.next
            }
            return array.toArray
        }
        sortedArrayToBST(listToArray(head))
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
      * Leetcode 145
      * Given a binary tree, return the postorder traversal of its nodes' values.
      * Recrusively
      * @param root
      * @return
      */
    def rcPostorderTraversal(root: TreeNode): List[Int] = {
        val list =new ListBuffer[Int]()
        def helper(root: TreeNode): Unit = {
            if (root == null) return
            helper(root.left)
            helper(root.right)
            list.append(root.value)
        }
        helper(root)
        list.toList
    }

    /**
      * Leetcode 145
      * Given a binary tree, return the postorder traversal of its nodes' values.
      * @param root
      * @return
      */
    def postorderTraversal(root: TreeNode): List[Int] = {
        var stack = List[TreeNode]()
        val list =new ListBuffer[Int]()
        var temp = root
        while (temp != null || stack.nonEmpty) {
            if (temp != null) {
                temp.value +=: list  
                stack = temp :: stack
                temp = temp.right
            } else {
                temp = stack.head.left
                stack = stack.tail
            }
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
      * Leetcode 111
      * Given a binary tree, find its minimum depth.
      * BFS Algorithm
      * @param root
      * @return
      */
    def bfsMinDepth(root: TreeNode): Int = {
        val queue = new Queue[TreeNode]()
        if (root == null) return 0
        if (root.left == null && root.right == null) return 1
        queue.enqueue(root)
        var depth = 0
        while (queue.nonEmpty){
            depth += 1
            (1 to queue.size).foreach(x =>{
                val curNode = queue.dequeue()
                // the left and right child nodes of the node are null, the node is at the minimum depth.
                if (curNode.left == null &&  curNode.right == null) return depth
                if (curNode.left != null) queue.enqueue(curNode.left)
                if (curNode.right != null) queue.enqueue(curNode.right)
            })
        }
        return depth
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

    /**
      * Leetcode 114
      * Given a binary tree, flatten it to a linked list in-place.
      * Recrusively
      * @param root     - tree node
      */
    def rcFlatten(root: TreeNode): Unit = {
        var pre: TreeNode = null
        def helper(root: TreeNode) : Unit = {
            if (root == null) return
            helper(root.right)
            helper(root.left)
            root.right = pre
            root.left = null
            pre = root
        }
        helper(root)
    }

    /**
      * Leetcode 114
      * Given a binary tree, flatten it to a linked list in-place.
      * Iteratively
      * @param root     - tree node
      */
    def flatten(root: TreeNode): Unit = {
        var stack = List[TreeNode]()
        var cur = root
        var pre: TreeNode = null 
        while (stack.nonEmpty || cur != null) {
            while (cur != null) {
                stack = cur :: stack
                cur = cur.right
            }
            cur = stack.head
            // if current node hasn't left node or it's right node have been traversed 
            if (cur.left == null || cur.left == pre) {
                stack = stack.tail
                cur.right = pre
                cur.left = null
                pre = cur
                cur = null
            } else {
                cur = cur.left
            }
        }       
    }
    
    /**
      * Leecode 124
      * Given a non-empty binary tree, find the maximum path sum.
      * For this problem, 
      * a path is defined as any sequence of nodes from some starting node to any node in the tree along the parent-child connections.
      * The path must contain at least one node and does not need to go through the root.
      * @param root
      * @return     - Int max path num
      */
    def maxPathSum(root: TreeNode): Int = {
        var max = Int.MinValue
        def helper(root: TreeNode): Int = {
            if (root == null) return 0
            val leftMax = Math.max(helper(root.left), 0)
            val rightMax = Math.max(helper(root.right), 0)
            max = Math.max(max, root.value + leftMax + rightMax)
            return root.value + Math.max(leftMax, rightMax)
        }
        helper(root)
        max
    }

    /**
      * Leetcode 199
      * Given a binary tree, imagine yourself standing on the right side of it,
      * return the values of the nodes you can see ordered from top to bottom.
      * Example：
      * input： [1,2,3,null,5,null,4]
      * output： [1, 3, 4]
      * @param root
      * @return
      */
    def rightSideView(root: TreeNode): List[Int] = {
        val queue = new Queue[TreeNode]()
        val list = new ListBuffer[Int]()
        if (root == null) return list.toList
        queue.enqueue(root)
        while (queue.nonEmpty) {
            list.append(queue.last.value)
            queue.indices.foreach(_ =>{
                val node = queue.dequeue()
                if (node.left != null) queue.enqueue(node.left)
                if (node.right != null) queue.enqueue(node.right)
            })
        }
        list.toList
    }

    /**
      * Leetcode 222
      * Given a complete binary tree, count the number of nodes.
      * In a complete binary tree every level, 
      * except possibly the last, is completely filled, 
      * and all nodes in the last level are as far left as possible. 
      * It can have between 1 and 2h nodes inclusive at the last level h.
      * @param root
      * @return
      */
    def countNodes(root: TreeNode): Int = {
        if (root == null) return 0
        var num = 0
        val queue = new Queue[TreeNode]
        queue.enqueue(root)
        while (queue.nonEmpty) {
            num += queue.size
            val levelnum = (1 to queue.size).foldLeft() { (_, _) =>
                val temp = queue.dequeue
                if (temp.left != null) queue.enqueue(temp.left)
                if (temp.right != null) queue.enqueue(temp.right)
            }            
        }
        return num
    }

    def rcCountNodes(root: TreeNode): Int = {
        if(root == null)
        return 0
        1 + rcCountNodes(root.left) + rcCountNodes(root.right)
    }

    /**
      * Leetcode 226
      * Invert a binary tree.
      * " Google: 90% of our engineers use the software you wrote (Homebrew), 
      * but you can’t invert a binary tree on a whiteboard so f*** off.  "
      * @param root
      * @return
      */
    def invertTree(root: TreeNode): TreeNode = {
        if (root == null) return null
        val right = invertTree(root.right)
        val left = invertTree(root.left)
        root.right = left
        root.left = right
        root
    }

    /**
      * Leetcode 226
      * Invert a binary tree. Iteratively
      * @param root
      * @return
      */
    def iterInvertTree(root: TreeNode): TreeNode = {
        if (root == null) return null
        val queue = new Queue[TreeNode]
        queue.enqueue(root)
        while (queue.nonEmpty) {
            for (i <- 0 until queue.size){
                val current = queue.dequeue
                val temp = current.left
                current.left = current.right
                current.right = temp
                if (current.left != null) queue.enqueue(current.left)
                if (current.right != null) queue.enqueue(current.right)
            }            
        }
        root
    }
    /**
      * Leetcode 235
      * Given a binary search tree (BST), find the lowest common ancestor (LCA) of two given nodes in the BST.
      * The lowest common ancestor is defined between two nodes p and q as the lowest node in T that has both p and q as descendants 
      * (where we allow a node to be a descendant of itself
      * @param root
      * @param p
      * @param q
      * @return
      */
    def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
        // if (p.value > root.value && q.value > root.value) return lowestCommonAncestor(root.right, p, q)
        // if (p.value < root.value && q.value < root.value) return lowestCommonAncestor(root.left, p, q)
        // return root
        Option(root).fold(root) { root =>
            if (root.value > q.value && root.value > p.value) {
                lowestCommonAncestor(root.left, p, q)
            } else if (root.value < q.value && root.value < p.value) {
                lowestCommonAncestor(root.right, p, q)
            } else {
                root
            }
        }
    }

    def iterLowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
        var node = root
        while (node != null) {
            if (p.value > node.value && q.value > node.value) {
                node = node.right
            }else if (p.value < node.value && q.value < node.value){
                node = node.left
            }else {
                return node
            }
        }
        return null
    }

    /**
      * Leetcode 236
      * Given a binary tree, find the lowest common ancestor (LCA) of two given nodes in the tree.
      * @param root
      * @param p
      * @param q
      * @return
      */
    def lowestCommonAncestorBT(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
        import scala.collection.mutable.{Map, Set}
        val parent: Map[Int, TreeNode] = Map()
        val visited = Set[Int]()
        //save nodes' parent nodes
        def dfs(root: TreeNode): Unit = {
            if (root.left != null) {
                parent += (root.left.value -> root)
                dfs(root.left)
            }
            if (root.right != null) {
                parent += (root.right.value -> root)
                dfs(root.right)
            }
        }
        dfs(root)
        var pNode = Option(p)
        var qNode = Option(q)
        // save visited node into visited set from p to its ancestors
        while (pNode != None) {
            visited.add(pNode.get.value)
            pNode = parent.get(pNode.get.value)
        }
        // from q to its ancestors,find visited ancestor
        while (qNode != None) {
            if (visited.contains(qNode.get.value)) return qNode.get
            qNode = parent.get(qNode.get.value)
        }
        return null
    }
    
    /**
      * Leetcode 538 & Leetcode 1038
      * Given a Binary Search Tree (BST), 
      * convert it to a Greater Tree such that every key of the original BST is changed to the original key plus sum of all keys greater than the original key in BST.
      * @param root
      * @return
      */
    def bstToGst(root: TreeNode): TreeNode = {
        var sum = 0
        def helper(root: TreeNode): TreeNode = {
            if (root != null) {
                helper(root.right)
                sum += root.value
                root.value = sum
                helper(root.left)
            }
            return root
        }
        helper(root)
    }

    /**
      * Leetcode 1382
      * Given a binary search tree, return a balanced binary search tree with the same node values.
      * @param root
      * @return
      */
    def balanceBST(root: TreeNode): TreeNode = {
        val array = new ArrayBuffer[TreeNode]()
        def inorder(root: TreeNode): Unit = {
            if (root == null) return
            inorder(root.left)
            array.append(root)
            inorder(root.right)
        }
        inorder(root)
        def build(left: Int, right: Int): TreeNode = {
            if (left > right) return null
            val cur:Int = (left + right) / 2
            val node = array(cur)
            node.left = build(left, cur - 1)
            node.right = build(cur + 1, right)
            node
        }
        if (array.length < 3) return root
        build(0, array.length - 1)
    }
}
