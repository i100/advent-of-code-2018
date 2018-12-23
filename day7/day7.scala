import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Edge(source: Char, dest: Char)
class Node(val id: Char) { 
  var parents: ListBuffer[Node] = ListBuffer[Node]()
  var children: ListBuffer[Node] = ListBuffer[Node]() 
  override def toString(): String = { s"Node id: $id" } //, children: $children, parents: $parents" }
}

class DAG(edges: List[Edge]) {
  def buildFromEdges(): (List[Node], List[Node], Map[Char, Node]) = {
    var procdNodes: Map[Char, Node] = Map[Char, Node]()
    def findRoot() = { }

    for (edge <- edges) {
      val sourceID: Char = edge.source
      val destID: Char = edge.dest 
      if (! procdNodes.contains(sourceID)) procdNodes += (sourceID -> new Node(sourceID))
      if (! procdNodes.contains(destID)) procdNodes += (destID -> new Node(destID))
      procdNodes(sourceID).children += procdNodes(destID)
      procdNodes(destID).parents += procdNodes(sourceID)
    }

    val rootNodes = procdNodes.filter { _._2.parents.length == 0}.values.toList
    val leafNodes = procdNodes.filter { _._2.children.length == 0}.values.toList
    
    (rootNodes, leafNodes, procdNodes)
  }
  
  lazy val (roots: List[Node], leaves: List[Node], nodes: Map[Char, Node]) = buildFromEdges
  lazy val rawNodes: Set[Node] = {
    def nextNode(remainingNodes: List[Node], procdNodes: Set[Node]): Set[Node] = {
      remainingNodes match {
        case Nil => procdNodes
        case head :: tail => nextNode(tail, procdNodes + head)
      }
    }
    nextNode(nodes.toList.map(_._2), Set())
  }  

  def traverse() = { }
  def search() = { }
  def isRoot() = { }
  def isLeaf(node: Node) = { if (node.children.length == 0) true else false }

  // oops I solved the wrong problem...
  def findLongestPaths(): Map[Node, Int] = {
    def nextLayer(layer: List[Node], prevLayers: List[(Node, Int)], 
        iter: Int): List[(Node, Int)] = {
      val procdLayers = layer.map((_, iter)) ::: prevLayers

      layer.map(_.children).flatten match {
        case l if l.length > 0 => nextLayer(l, procdLayers, iter + 1)
        case _ => procdLayers
      }
    }
  nextLayer(roots, List[(Node, Int)](), 0)
    .groupBy(_._1)
    .mapValues { n => 
      n.foldLeft(0) { (acc, v) => if (v._2 > acc) v._2 else acc }
    }
  }

  def findActivationOrder() = {
    def findNextActiveNode(activeOrder: List[Node], inactivePool: Set[Node]): List[Node] = {
      inactivePool.size match {
        case 0 => activeOrder.reverse
        case _ =>
          val nextActivePool = inactivePool.filter { n =>
            (n.parents.toSet & inactivePool).size == 0
          }.toList.sortWith(_.id < _.id)
          println(s"next active pool: $nextActivePool")
          val nextActiveNode = nextActivePool(0)
          println(s"next active node: $nextActiveNode")
          val nextInactivePool = inactivePool - nextActiveNode
          println(s"next inactive pool: $nextInactivePool")
          findNextActiveNode(nextActiveNode :: activeOrder, nextInactivePool) 
      }
    }
    findNextActiveNode(List(), rawNodes)
  }
}


val inputPattern = raw"^S.+([A-Z]).+([A-Z]).+".r

val inputEdges = Source.fromFile("./input")
  .getLines
  .toList
  .map { line =>
    val inputPattern(source, dest) = line 
    Edge(source(0), dest(0))
  }

val dag: DAG = new DAG(inputEdges)
dag.buildFromEdges
//println(s"${dag.buildFromEdges}")
/*
val sortedOrder = dag.findLongestPaths.toList
  .map { n => (n._1.id, n._2) }
  .sortWith { (n1: (Char, Int), n2: (Char, Int)) => 
    if ((n1._2 < n2._2) || (n1._2 == n2._2 && n1._1 < n2._1)) true else false }
  .map { n => n._1 }
  .mkString
println(sortedOrder)
*/
val sortedOrder = dag.findActivationOrder.map(_.id)
println(sortedOrder.mkString)