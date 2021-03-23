object Pigeonhole {
    
   def main(args: Array[String]) {
      
      val result1 = generateAllocList (17, 7)
      println (result1)
      
      val result2 = generateAllocList (4, 7)
      println (result2)
      
      val result3 = generateAllocList (14, 7)
      println (result3)
      
      val result4 = generateAllocList (12, 7)
      println (result4)

   } // main

   //---------------------------------------------------------------
    def generateAllocList (taskTotal: Int, coreTotal: Int): List [List [Int]] = {
       
        val t = taskTotal   // e.g. 17
        val c = coreTotal   // e.g. 7
       
        if (t < c) { // Fewer tasks than cores
            generateBucketRange (1, 1, t)
        }
        else if ((t % c) == 0) { // Tasks are split evenly between cores
            generateBucketRange (1, t/c, c)
        }
       else {   
            // Tasks are split unevenly between cores: some cores have an extra task
            // Cores with extra tasks are referred to as 'heavy' in the following code.
            val diff = t % c
            val div  = t/c  // Integer division, so 17/7 == 2
            val heavy = generateBucketRange (1, 1 + div, diff)
            // Example: [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
            
            val start = 1 + (1 + div) * diff
            val light = generateBucketRange (start, div, c - diff)   
            // Example: [[10, 11], [12, 13], [14, 15], [16, 17]]
            
            heavy ++ light // Combine the two lists
       }
   } // generateAllocList
   
   //---------------------------------------------------------------
  def generateBucketRange (start: Int, bucketPopulation: Int, numBuckets: Int) : List [List [Int]] = {
            
    val s = start               // Starting number in first bucket
    val c = bucketPopulation    // Number of integers in each bucket
    val n = numBuckets          // Total number of buckets
      
    // Example ans == Vector (List (10, 11), List (12, 13))
    val ans = for (i <- 1 to n) yield {
          val a = s + (i-1) * c
          val z = a + c
          List.range (a, z) // z is not included in range
    }
    ans.toList // Example return: List (List (10, 11), List (12, 13))
    
  } // generateBucketRange
}
