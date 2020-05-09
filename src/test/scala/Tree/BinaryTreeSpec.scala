package Tree
import org.scalatest.FlatSpec

class BinaryTreeSpec extends FlatSpec{

  val preOrder = Array(3,9,20,15,7)
  val inOrder = Array(9,3,15,20,7)
  val postOrder = Array(9,15,7,20,3)
  val root = BinaryTree.buildFromPreIn(preOrder,inOrder)

  "BinaryTree rcSerialize" should "return a serialized String passed to it" in {
    assert(BinaryTree.rcserialize(root) === "3,9,null,null,20,15,null,null,7,null,null")
  }

  "BinaryTree level serialize" should "return a serialized String passed to it" in {
    val preOrder = Array(1,2,3,4,5)
    val inOrder = Array(2,1,4,3,5)
    assert(BinaryTree.serialize(BinaryTree.buildFromPreIn(preOrder,inOrder)) === "[1,2,3,null,null,4,5]")
  }

  "BinaryTree inorder prester build tree" should "return a String passed to it" in {  
    assert(BinaryTree.serialize(root) === "[3,9,20,null,null,15,7]")
  }

  "BinaryTree inorder poster build tree" should "return a String passed to it" in {  
    assert(BinaryTree.serialize(BinaryTree.buildFromInPost(inOrder,postOrder)) === "[3,9,20,null,null,15,7]")
  }

  "BinaryTree unique BST's number" should "return a Int passed to it" in {  
    assert(BinaryTree.numTrees(3) === 5)
  }

  "BinaryTree unique BST" should "return a List passed to it" in { 
    val list = for {node <- BinaryTree.generateTrees(3)} yield BinaryTree.serialize(node)
    assert(list.mkString("[",",","]") === "[[1,null,2,null,3],[1,null,3,2],[2,1,3],[3,1,null,null,2],[3,2,null,1]]")
  }

  "BinaryTree valid BST" should "return a Boolean passed to it" in { 
    assert(BinaryTree.isValidBST(root) === false)
    assert(BinaryTree.inorderValidBST(root) === false)
    val preOrder = Array[Int](2,1,3)
    val inOrder = Array[Int](1,2,3)
    assert(BinaryTree.isValidBST(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
    assert(BinaryTree.inorderValidBST(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
  }

  "BinaryTree recover BST" should "return a recovered tree passed to it" in { 
    val preOrder = Array[Int](1, 3, 2)
    val inOrder = Array[Int](3, 2, 1)
    val root = BinaryTree.buildFromPreIn(preOrder,inOrder)
    val rcRoot = BinaryTree.buildFromPreIn(preOrder,inOrder)
    BinaryTree.recoverTree(root)
    BinaryTree.rcRecoverTree(rcRoot)
    assert(BinaryTree.serialize(root) === "[3,1,null,null,2]")
    assert(BinaryTree.serialize(rcRoot) === "[3,1,null,null,2]")
  }

  "BinaryTree level traversal" should "return a List" in {  
    val tree: List[List[Int]] =
        List(
            List(3),
            List(9,20),
            List(15,7)
        )
    assert(BinaryTree.levelOrder(root) === tree)
  }

  "BinaryTree recursive max depth" should "return a Int passed to it" in {
    assert(BinaryTree.rcmaxDepth(root) === 3)
  }

  "BinaryTree max depth" should "return a Int passed to it" in {
    assert(BinaryTree.rcmaxDepth(root) === 3)
  }

  "BinaryTree recursive min depth" should "return a Int passed to it" in {
    assert(BinaryTree.rcMinDepth(root) === 2)
  }

  "BinaryTree BFS min depth" should "return a Int passed to it" in {
    assert(BinaryTree.bfsMinDepth(root) === 2)
  }

  "Recrusively Symmetric Tree" should "return a boolean passed to it" in {
    val preOrder = Array[Int](1,2,2)
    val inOrder = Array[Int](2,1,2)
    assert(BinaryTree.rcSymmetric(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
  }

  "Iteratively Symmetric Tree" should "return a boolean passed to it" in {
    val preOrder = Array[Int](1,2,2)
    val inOrder = Array[Int](2,1,2)
    assert(BinaryTree.isSymmetric(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
  }

  "zigzag level order" should "return a List" in {  
    val tree: List[List[Int]] =
        List(
            List(3),
            List(20,9),
            List(15,7)
        )
    assert(BinaryTree.zigzagLevelOrder(root) === tree)
  }

  "BinaryTree rcInorder" should "return a List passed to it" in {
    val list = List[Int](1,3,2)
    val preOrder = Array(1,2,3)
    val inOrder = Array(1,3,2)
    assert(BinaryTree.rcInorderTraversal(BinaryTree.buildFromPreIn(preOrder,inOrder)) === list)
  }

  "BinaryTree Inorder" should "return a List passed to it" in {
    val list = List[Int](1,3,2)
    val preOrder = Array(1,2,3)
    val inOrder = Array(1,3,2)
    assert(BinaryTree.inorderTraversal(BinaryTree.buildFromPreIn(preOrder,inOrder)) === list)
  }

  "BinaryTree isBalanced" should "return a Boolean passed to it" in {
    assert(BinaryTree.isBalanced(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
  }

  "Build binaryTree from a sort array" should "return a Tree passed to it" in {  
    assert(BinaryTree.serialize(BinaryTree.sortedArrayToBST(Array[Int](-10,-3,0,5,9))) === "[0,-10,5,null,-3,null,9]")
  }

  "Build binaryTree from a sort list" should "return a Tree passed to it" in { 
    // convert array to list [-10,-3,0,5,9]
    def setListNode(array: Array[Int]): ListNode = {
        val nullHead:ListNode = new ListNode
        array.foldLeft(nullHead)((node:ListNode, num:Int) => {
            val item = new ListNode(num)
            node.next = item
            item
        })
        nullHead.next
    }
    assert(BinaryTree.serialize(BinaryTree.sortedListToBST(setListNode(Array[Int](-10,-3,0,5,9)))) === "[0,-10,5,null,-3,null,9]")
  }

}
