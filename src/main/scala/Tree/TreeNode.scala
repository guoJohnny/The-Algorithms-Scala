package Tree

class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
}
class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
    /**
      * create list of node for test
      * 
      * @param x    - num of node
      * @return     - next node's pointer
      */
    def setNextNode(x: Int): ListNode = {
        val nextNode = new ListNode(x)
        next = nextNode
        nextNode
    }
}