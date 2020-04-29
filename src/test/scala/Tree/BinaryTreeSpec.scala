package Tree
import org.scalatest.FlatSpec

class BinaryTreeSpec extends FlatSpec{
  
    "BinaryTree rcSerialize" should "return a serialized String passed to it" in {
    val preOrder = Array(3,9,20,15,7)
    val inOrder = Array(9,3,15,20,7)
    assert(BinaryTree.rcserialize(BinaryTree.buildFromPreIn(preOrder,inOrder)) === "3,9,null,null,20,15,null,null,7,null,null")
  }

   "BinaryTree level serialize" should "return a serialized String passed to it" in {
    val preOrder = Array(1,2,3,4,5)
    val inOrder = Array(2,1,4,3,5)
    assert(BinaryTree.serialize(BinaryTree.buildFromPreIn(preOrder,inOrder)) === "[1,2,3,null,null,4,5]")
  }

  "BinaryTree inorder prester build tree" should "return a String" in {  
    val inOrder = Array(9,3,15,20,7)
    val preOrder = Array(3,9,20,15,7)
    assert(BinaryTree.serialize(BinaryTree.buildFromPreIn(preOrder,inOrder)) === "[3,9,20,null,null,15,7]")
  }

  "BinaryTree inorder poster build tree" should "return a String" in {  
    val inOrder = Array(9,3,15,20,7)
    val postOrder = Array(9,15,7,20,3)
    assert(BinaryTree.serialize(BinaryTree.buildFromInPost(inOrder,postOrder)) === "[3,9,20,null,null,15,7]")
  }

  "BinaryTree level traversal" should "return a List" in {  
    val inOrder = Array(9,3,15,20,7)
    val postOrder = Array(9,15,7,20,3)
    val tree: List[List[Int]] =
        List(
            List(3),
            List(9,20),
            List(15,7)
        )
    assert(BinaryTree.levelOrder(BinaryTree.buildFromInPost(inOrder,postOrder)) === tree)
  }

  "BinaryTree recursive max depth" should "return a Int passed to it" in {
    val preOrder = Array(3,9,20,15,7)
    val inOrder = Array(9,3,15,20,7)
    assert(BinaryTree.rcmaxDepth(BinaryTree.buildFromPreIn(preOrder,inOrder)) === 3)
  }
  "BinaryTree max depth" should "return a Int passed to it" in {
    val preOrder = Array(3,9,20,15,7)
    val inOrder = Array(9,3,15,20,7)
    assert(BinaryTree.rcmaxDepth(BinaryTree.buildFromPreIn(preOrder,inOrder)) === 3)
  }
}
