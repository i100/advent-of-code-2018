import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.{ Try, Success, Failure }

val inputPattern = raw"^S.+([A-Z]).+([A-Z]).+".r

val inputEdges = Source.fromFile("./input")
  .getLines
  .toList
  .map { line =>
    val inputPattern(source, dest) = line 
    Edge(source(0), dest(0))
  }


case class Edge(source: Char, dest: Char)

object Node {
  def getLifetime(id: Char): Int = { 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(id) + 61
  }
  def getLifetime(node: Node): Int = { getLifetime(node.id) }
}

class Node(val id: Char) { 
  var parents: ListBuffer[Node] = ListBuffer[Node]()
  var children: ListBuffer[Node] = ListBuffer[Node]() 
  override def toString(): String = { s"Node id: $id" } //, children: $children, parents: $parents" }
  def isRoot() = { if (parents.length == 0) true else false }
  def isLeaf() = { if (children.length == 0) true else false }
}

class DAG(edges: List[Edge]) {
  def buildFromEdges(): (List[Node], List[Node], Map[Char, Node]) = {
    var procdNodes: Map[Char, Node] = Map[Char, Node]()

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

  /*
  def traverse() = { }
  def search() = { }
  */

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

  private var activationOrder: List[Node] = List()
  private var completedPool: Set[Node] = Set()
  private var activatingPool: Set[Node] = Set()
  private var inactivePool: Set[Node] = rawNodes

  def nextActivatedNode(inactive: Set[Node]) = {
    inactive.filter { n =>
      (n.parents.toSet & completedPool).size == n.parents.length
    }.toList.sortWith(_.id < _.id)(0)
  }
  
  def nextActivatedNode(): Node = { nextActivatedNode(inactivePool) }

  def popNextActivatingNode(): Node = {
    val nextNode = nextActivatedNode
    inactivePool -= nextNode 
    activatingPool += nextNode
    nextNode
  }

  // part 1
  def findP1ActivationOrder() = {
    def findNextActiveNode(activated: List[Node], inactive: Set[Node]): List[Node] = {
      inactive.size match {
        case 0 => activated.reverse
        case _ =>
          val nextActiveNode = nextActivatedNode(inactive)
          findNextActiveNode(nextActiveNode :: activated, inactive - nextActiveNode) 
      }
    }
    findNextActiveNode(activationOrder, inactivePool)
  }

  def completeActivation(completedNodes: List[Node]): List[Node] = {
    activationOrder = completedNodes ::: activationOrder 
    completedPool ++= completedNodes.toSet
    activatingPool --= completedNodes.toSet
    activationOrder
  }
}

// part two
case class Worker(val node: Node, val lifetime: Int, val createdAt: Int)
case class TickState(second: Int, workers: List[Worker])
val MAX_WORKERS = 5

class Ticker() {
  val dag = new DAG(inputEdges)
  var history: List[TickState] = List[TickState]()

  def tick(prevTick: TickState): TickState = {
    history = prevTick :: history
    val curSecond = prevTick.second + 1
    val completedWorkers = prevTick.workers.filter { w => 
      (curSecond - w.createdAt) == w.lifetime
    }
    val activeWorkers = prevTick.workers.filter { w => ! completedWorkers.contains(w) }

    if (curSecond % 3 == 0) {
      println("tick...")
      println(s"second: $curSecond")
      println(s"active workers: ${activeWorkers.map(_.node.id)}")
      println(s"completed workers: ${completedWorkers.map(_.node.id)}")
    }

    dag.completeActivation(completedWorkers.map(_.node)) 

    def createWorker(node: Node): Worker = { Worker(node, Node.getLifetime(node), curSecond) }

    def fillWorkers(active: List[Worker]): List[Worker] = {
      active.length match {
        case n if n == MAX_WORKERS => active 
        case _ => 
          Try(dag.popNextActivatingNode) match {
            case Failure(e) => active 
            case Success(v) => fillWorkers(createWorker(v) :: active)
          }
      }
    }

    val curActiveWorkers = if (activeWorkers.length < MAX_WORKERS) {
      fillWorkers(activeWorkers)
    } else {
      activeWorkers
    }

    TickState(curSecond, curActiveWorkers)
  }

  def run() = {
    val initialState = TickState(-1, List[Worker]())

    def nextTick(state: TickState): TickState = {
      tick(state) match {
        case nextState if nextState.workers.length == 0 => state
        case nextState => nextTick(nextState)
      }
    }

    nextTick(initialState)
  }
}


val DO_PART_1 = false 
val DO_PART_2 = true 

if (DO_PART_1) {
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
  val sortedOrder = dag.findP1ActivationOrder.map(_.id)
  println(sortedOrder.mkString)
}

if (DO_PART_2) {
  val ticker = new Ticker 
  ticker.run
  println(s"Ticker ran for ${ticker.history.length - 1} ticks")
}