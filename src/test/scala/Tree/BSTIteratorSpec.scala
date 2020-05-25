package Tree
import org.scalatest.FlatSpec

class BSTIteratorSpec extends FlatSpec{
    var obj = new Codec()

    "Binary Search Tree Iterator" should "be instantiated and called as such" in {
    val root:TreeNode = obj.deserialize("[7,3,15,null,null,9,20]")
    var iter = new BSTIterator(root)
    var param_1 = iter.next()
    var param_2 = iter.hasNext()
    assert(param_1 === 3)
    assert(param_2 === true)
    val stackIter = new BSTStackIterator(root)
    var param_3 = stackIter.next()
    var param_4 = stackIter.hasNext()
    assert(param_3 === 3 && param_4 === true)
  }
}
