
/**
  Author: Philron Hozier
  CMPSCI240HW5
  Bayesian Networks
  Solution.scala
**/
import scala.collection.immutable.List
import java.util.NoSuchElementException


/**
  To execute, run in sbt w/ 'test' command
  Alternatively, run in scala REPL w/ 'load' command
**/
object SUM {
    final val NO_VALUE:Double = 1

  // case class args:

  // @args: B means P(B = b)
    final val b:Double = NO_VALUE 

  // @args: G means P(G = g)
    final val g:Double = NO_VALUE 

  // @args: W_BD means P(W = w | B = b, D = d)
    final val w:Double = NO_VALUE 

  // @args: D_BG means P(D = d | B = b, G = g)
    final val d:Double = NO_VALUE 

  // @args: F_D means (F = f | D = d)
    final val f:Double = NO_VALUE 

  // @args: R_D means (R = r | D = d)
    final val r:Double = NO_VALUE 

  // overview: creates a data structure to contain a joint probability 
  // and initializes all class variables to a default probability of 1

  var generate:List[Double] = List[Double]()
  var casing = ""
  for(k <- 0 to 5){
    generate = generate :+ NO_VALUE
    casing += "x"+k+", " 
  }

  casing = casing.substring(0, casing.length-2)

  // http://stackoverflow.com/questions/15349439/how-to-append-or-prepend-an-element-to-a-tuple-in-scala
  val tupadd = for (n <- 2 to 20) yield {
    val t = (0 until n).map(i => ('A'+i).toChar).mkString(", ")
    val u = ('A'+n).toChar
    val i = (0 until n).map(i => "x._"+(i+1)).mkString(", ")
    List(
      s"implicit class TupOps$n[$t](val x: ($t)) extends AnyVal {",
      s"  def :+[$u](y: $u) = ($i, y)",
      s"  def +:[$u](y: $u) = (y, $i)",
      s"  def :+[$u](a: $u) = ($i, a)",
      s"  def +:[$u](a: $u) = (a, $i)",
      "}"
    ).mkString("\n")
  }


  implicit class TupOps2[A, B](val x: (A, B)) extends AnyVal {
    def :+[C](y: C) = (x._1, x._2, y)
    def +:[C](y: C) = (y, x._1, x._2)
    def :+[C, D](y: C, a: D) = (x._1, x._2, y, a)
    def +:[C, D](y: C, a: D) = (y, x._1, x._2, a)
  }



  printf("==>     (%s)", (1,1) :+ 1:+ 1)


  // BUILD(generate)

  case class Probability(B:Double = b, G:Double = g, W_BD:Double = w, D_BG:Double = d, F_D:Double = f , R_D:Double = r)
  
  // overview: computes the Joint Query by finding product of all random variable values
  def f(the_probability_of:Probability):Double = {
    return (the_probability_of.B * the_probability_of.G * the_probability_of.W_BD * 
      the_probability_of.D_BG * the_probability_of.F_D * the_probability_of.R_D)
  }


  // overview: assists in computing the Marginal Query and Conditional Query
  // sums the joint probability of all the variables over the possible configurations of the unobserved variables
  def SGMA(products:List[Double]):Double = { 
    products match {
      case Nil =>  0
      case (h :: t) => h + SGMA(t)
    }
  }

  
  // def BUILD(lst:List[Double]):(Double,Double,Double,Double,Double,Double) = { 
  //   lst match {
  //     case (h :: t) => h :+ BUILD(t)
  //     case Nil =>  0
  //   }
  // }


  //overview: computes the complement of value
  def complement(X:Double) = 1 - X

  //<------------------------------------- question:  1.3 --------------------------------->
  //P(D=T | B=T, G=g ) 
  // Introducing the algorithm.
  // overview: find the products of each joint probability.
  // procedure: 1) create new probability objects modeling P(D=T | B=T, G=g )
  // each unobserved will be a new probability object. 2) Call the method .map on 
  // the List of Probability type. 3) FOR EACH probabilty object IN THE LIST, 
  // .map will compute the product of all factors residing in probabilty object. 
  //  4) When .map takes the method f as a parameter, this multiplicative functionality is applied.
  // i.e. (B=T * G=T * W=T * D=T * F=T * R=T) finds the product of all true probabilites 
  // written into a probabilty object. 5) .map(f) returns a new List of type Double. This
  // new list hold all the joint probabilites FOR EACH probabilty object IN THE probability LIST
   
  val b_isTrue = (List() 
           :+ new Probability(B = 0.05 , G = 0.10 ,               D_BG =   0.85)).map(f)
  val b_isFalse = (List() 
           :+ new Probability(B = complement(0.05) , G = 0.10 ,   D_BG =  .20)).map(f)

  
  val bT_x_G = 0.05 * 0.10 // summing across (B = b; where B = T)
  val bF_x_G = (complement(0.05)) * 0.10 // summing across (B = b; where B = F)

  // procedure: Use the SGMA (stands for Sigma meaning sum) method to find the sum of all the products. 
  // The list of computed products, otherwise known as the 
  // joint probabilites is sent in as a paramenter for the SGMA method.
  println("question 1.3: " + SGMA(b_isTrue. union (b_isFalse)) / (bT_x_G + bF_x_G)) // summate / and divide: the conditional p. query.
  // Note: Conditional Query =====>SIGMA( P(D| G = T, B=b) * P(B=b) * P(G=T) / SIGMA(P(B=b)*P(=G=T)))




  //<------------------------------------- question:  2.3 --------------------------------->
  //P(R=T, F=T, W=T, D=T, B=T, G=g | B=T) <== Conditional ==> P(a|b) = P(a intersect b) / P(b). 
  var foo:List[Probability] = (List()
            :+ new Probability(B = 0.05 , G = 0.10 , W_BD = (0.90), D_BG = 0.85, F_D = (0.95), R_D = 0.70)
            :+ new Probability(B = 0.05 , G = (complement(0.10)) , W_BD = (0.90), D_BG = 0.45, F_D = (0.95), R_D = 0.70))
  val products = foo.map(f)

  //<------------------------------------- question:  2.2 --------------------------------->

  //P(B=b, G=g , W=F, D=T , F=T, R=T | D) / [ P(D=T) + P(D=T) ]
  var bar:List[Probability] = (List() 
             :+ new Probability(0.05, 0.10, (complement(0.90)), 0.85, (0.95), 0.70)
             :+ new Probability(0.05, (complement(0.10)), (complement(0.90)), 0.45, (0.95), 0.70)
             :+ new Probability((complement(0.05)), 0.10, (complement(0.75)), 0.20, (0.95), 0.70)
             :+ new Probability((complement(0.05)), (complement(0.10)), (complement(0.75)), 0.05, (0.95), 0.70 ))
  val the_products = bar.map(f)

  bar = (List() 
            :+ new Probability(0.05, 0.10, (complement(0.20)), complement(0.85), (0.10), 0.35)
            :+ new Probability(0.05, (complement(0.10)), (complement(0.20)), (complement(0.45)), (0.10), 0.35)
            :+ new Probability((complement(0.05)), 0.10, (complement(0.05)), (complement(0.20)), (0.10), 0.35)
            :+ new Probability((complement(0.05)), (complement(0.10)), (complement(0.05)), (complement(0.05)), (0.10), 0.35 ))
  val the_producer = bar.map(f)

  //<------------------------------------- question:  2.1 --------------------------------->
  // P(D=T | B=b, G=g)
  // We do not know B , G. They are unobserved. Hence. We must use marginal query and SUM across all possibilities.
  // I have computed the probability summing over the unobservables. So, in this case, it may also be considered a hybrid 
  // between the techniques of marginal and conditional queries.
  var D_TRUE:List[Probability] = (List() 
             :+ new Probability(0.05, 0.10, 0.85)
             :+ new Probability(0.05, (complement(0.10)), 0.45)
             :+ new Probability((complement(0.05)), 0.10, 0.20)
             :+ new Probability((complement(0.05)), (complement(0.10)), 0.05))


  //<------------------------------------- question:  1.4 --------------------------------->

  // P(B=T , G=T , W=T, D=T, F=T, R=T)
  var Q14:List[Probability] = List() :+ new Probability(0.05, 0.10, (0.90), 0.85, (0.95), 0.70)

  //<------------------------------------- question:  3.1 --------------------------------->

  // B is unobserved, G is unobserved, find probabililty for each symptom true, when other symptoms false.
  // Solve: (B=b , G=g , W_BD = ?, D_BG = T, F_D = ?, R_D = ?)


  // P(B=b , G=g , W=F, D=T, F=F, R= ?) =
  // P(B=b , G=g , W=F, D=T, F=F, R= T | D = d)
  var the_joint_probability_of_symptom:List[List[Double]] = List[List[Double]]()
  var denom:List[Double] = List()



  the_joint_probability_of_symptom = the_joint_probability_of_symptom :+ (List() 
             :+ new Probability(0.05, 0.10, (complement(0.90)), 0.85, (complement(0.95)), 0.70)
             :+ new Probability(0.05, (complement(0.10)), (complement(0.90)), 0.45, (complement(0.95)), 0.70)
             :+ new Probability((complement(0.05)), 0.10, (complement(0.75)), 0.20, (complement(0.95)), 0.70)
             :+ new Probability((complement(0.05)), (complement(0.10)), (complement(0.75)), 0.05, (complement(0.95)), 0.70 )).map(f)

  denom = denom :+ SGMA(the_joint_probability_of_symptom(0)) + SGMA((List() 
            :+ new Probability(0.05, 0.10, (complement(0.20)), complement(0.85), (complement(0.10)), 0.35)
            :+ new Probability(0.05, (complement(0.10)), (complement(0.20)), (complement(0.45)), (complement(0.10)), 0.35)
            :+ new Probability((complement(0.05)), 0.10, (complement(0.05)), (complement(0.20)), (complement(0.10)), 0.35)
            :+ new Probability((complement(0.05)), (complement(0.10)), (complement(0.05)), (complement(0.05)), (complement(0.10)), 0.35 )).map(f))



  // P(B=b , G=g , W=F, D=T, F= ?, R=F)
  the_joint_probability_of_symptom = the_joint_probability_of_symptom :+ (List()  
             :+ new Probability(0.05, 0.10, (complement(0.90)), 0.85, (0.95), (complement(0.70)))
             :+ new Probability(0.05, (complement(0.10)), (complement(0.90)), 0.45, (0.95), (complement(0.70)))
             :+ new Probability((complement(0.05)), 0.10, (complement(0.75)), 0.20, (0.95), (complement(0.70)))
             :+ new Probability((complement(0.05)), (complement(0.10)), (complement(0.75)), 0.05, (0.95), (complement(0.70)) )).map(f)

  denom = denom :+ SGMA(the_joint_probability_of_symptom(1)) + SGMA((List() 
            :+ new Probability(0.05, 0.10, (complement(0.20)), complement(0.85), (0.10), (complement(0.35)))
            :+ new Probability(0.05, (complement(0.10)), (complement(0.20)), (complement(0.45)), (0.10), (complement(0.35)))
            :+ new Probability((complement(0.05)), 0.10, (complement(0.05)), (complement(0.20)), (0.10), (complement(0.35)))
            :+ new Probability((complement(0.05)), (complement(0.10)), (complement(0.05)), (complement(0.05)), (0.10), (complement(0.35)) )).map(f))


  // P(B=b , G=g , W= ?, D=T, F=F, R=F)
  the_joint_probability_of_symptom = the_joint_probability_of_symptom :+ (List()
             :+ new Probability(0.05,       0.10,       (0.90), 0.85, (complement(0.95)), (complement(0.70)))
             :+ new Probability(0.05,       (complement(0.10)), (0.90), 0.45, (complement(0.95)), (complement(0.70)))
             :+ new Probability((complement(0.05)), 0.10,       (0.75), 0.20, (complement(0.95)), (complement(0.70)))
             :+ new Probability((complement(0.05)), (complement(0.10)), (0.75), 0.05, (complement(0.95)), (complement(0.70)) )).map(f)

  denom = denom :+ SGMA(the_joint_probability_of_symptom(2)) + SGMA((List() 
            :+ new Probability(0.05,        0.10,         (0.20), complement(0.85),  (complement(0.10)), (complement(0.35)))
            :+ new Probability(0.05,        (complement(0.10)),   (0.20), (complement(0.45)), (complement(0.10)), (complement(0.35)))
            :+ new Probability((complement(0.05)),  0.10,         (0.05), (complement(0.20)), (complement(0.10)), (complement(0.35)))
            :+ new Probability((complement(0.05)),  (complement(0.10)),   (0.05), (complement(0.05)), (complement(0.10)), (complement(0.35)) )).map(f))


  //<------------------------------------- question:  3.2 --------------------------------->
  // P(D=T, W=T) , B is unobserved, G is unobserved
  // find P(B=b, G=g | D = t, W = t) <==> conditional prob. law ==> P(a,b) | P(b) ==> conditional query
  // overview: find the products of each joint probability.
  val probabilities:List[Probability] = (List() 
             :+ new Probability(0.05,       0.10,       (0.90), 0.85)
             :+ new Probability(0.05,       (complement(0.10)), (0.90), 0.45)
             :+ new Probability((complement(0.05)), 0.10,       (0.75), 0.20)
             :+ new Probability((complement(0.05)), (complement(0.10)), (0.75), 0.05))



  //<------------------------------------- question:  3.3 --------------------------------->
  // P(B=? , G=? , W= ?, D=?, F=?, R=?)
  // overview: This is a list of joint probabilites that with differing combinations of factors being true
  // I attempt to find the most likely combination that will occur if a person is randomly selected.
  var Q33:List[Probability] = (List() :+ new Probability(0.05,      0.10,     (0.90),     0.85,     (0.95),     0.70)
                                      :+ new Probability(complement(0.05),  complement(0.10), (complement(0.90)), complement(0.85), (complement(0.95)), complement(0.70))
                                      :+ new Probability(0.05,      0.10,     (0.05),     0.05,     (0.10),     0.35)
                                      :+ new Probability(complement(0.05),  0.10,     (complement(0.05)), complement(0.20), (complement(0.10)), complement(0.35)) // B is false, greatest impact so far is finding sympotoms false with B is false.
                                      :+ new Probability(complement(0.05),  complement(0.10), (complement(0.05)), complement(0.05), (complement(0.10)), complement(0.35)) // try B and G is false
                                      :+ new Probability(complement(0.05),  complement(0.10), (complement(0.05)), complement(0.05), (complement(0.10)), 0.35) // try B and G is false
                                      )

  // Note: making just a single factor within the joint probabililty TRUE (P(R=T|D=F)) 
  // while all the other factors are FALSE drops the maximum probabilty found so far (0.451408) to (0.243066)
  // We can be safe in concluding that there exists no probability greater than 0.451408. 
  // This assures us that all other combinations of factors will not only be less than 0.451408.
  // This is important because the normalization axiom demands that the sum of probabilites = 1
  // We know that ( P(B=F , G=F , W=F, D=F, F=F, R=F) => 0.451408 
  //              + P(B=F , G=F , W=F, D=F, F=F, R=T) => 0.243066 = 0.694474) + remaining probabilites = 1
  // complement(0.69)4474 = 0.305526 means that there exist a finite amount of factor combinations that will produce
  // a probability of 0.305526 when summed.  This also means that a randomly selected person will appear to have:
  // B=F , G=F , W=F, D=F, F=F, R=F 45.1408% of the time.


  //<------------------------------- compute probabilities here --------------------------->
  // runs the computations through TestSuite.scala (type test command in sbt shell)
  def runnable():Unit = {
    printf("question 1.4: %.6f\n", SGMA(Q14.map(f)))
    printf("question 2.1: %f\n", SGMA(D_TRUE.map(f))) 
    printf("question 2.2: %f\n", SGMA(the_products)/ ((SGMA(the_products) + SGMA(the_producer)))) 
    printf("question 2.3: %f\n", SGMA(products)/foo(0).B) // conditional query. 
    
    // Note: Because the highest probability is R = T: 0.083200, we conclude that hair falling out
    // is most useful for diagnosing the disease (if a patient has only one symptom).
    printf("\ncases where R, F, W are individually true while other syms. false. unobserved: B=b, G=g \n" ) 
    for(i <- 0 to the_joint_probability_of_symptom.length - 1) {
          printf("question 3.1: %.6f \n", SGMA(the_joint_probability_of_symptom(i))/ denom(i) )

    }

    for(i <- 0 to probabilities.length - 1) {
          val the_product = List(probabilities(i)).map(f)
          var denominator:Double = 0.0

          for(i <- 0 to probabilities.length - 1){ 
            denominator += SGMA(List(probabilities(i)).map(f)) // for the denominator of P(B=b, G=g | D = t, W = t), sum across all found observables. this becomes our denominator.
          }
          def tuple(i:Int):(String, String)  = i match{
            case 1 => ("T","F")
            case 2 => ("F","T")
            case 3 => ("F","F")
            case _ => ("T","T")
          } 
          
          printf("question 3.2: when (B, G) = %s: \nthe product [P(B=b) * P(G=g) *P(D = t)*P(W = t)] ==> %.6f" 
                  +"/ \nSGMA([P(B=b) * P(G=g) *P(D = t)*P(W = t)]) ==> %.6f equals (%.6f/%.6f) = %.6f \n", tuple(i), SGMA(the_product), denominator, SGMA(the_product), denominator, SGMA(the_product)/ (denominator) )

    }

    println()
    for(s<- Q33.map(f)) printf("question 3.3: %.6f\n", s)
  }

}

