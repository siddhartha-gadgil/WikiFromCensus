object DistrictPages extends App {
   def rawfile = args(0)
   def file = if (rawfile.endsWith(".csv")) rawfile else rawfile+".csv"
   
   val district = new WikiFromCensus(file)
   district.saveAll
}