package scalocal

import collection.JavaConversions._

// scalac CompileResources.scala && scala CompileResources Test && scalac Test.scala Main.scala && scala Main
object CompileResources {
  import java.io._
  def read(file: File) = {
    import java.util.{Properties, Map}
    val p = new Properties
    val in = new FileInputStream(file)
    try { p.load(in) } 
    finally { in.close }
    (p.asInstanceOf[Map[String, String]]: collection.mutable.Map[String, String]).toMap
  }
  val baseNameRx = """(?:(.*?)\.)?([^.]+)""".r
  
  val defaultBaseName = "Resources"
  
  def listPropertiesFiles(baseName: String) = 
    new File(".").listFiles.filter(_.getName.matches(baseName.replaceAll("\\.", "\\.") + "(_.*?)?\\.properties"))
    
  def main(args: Array[String]) {
    var hasError = false
    
    val (fullClassName @ baseNameRx(packageName, simpleClassName), inputFiles) = args.toList match {
      case Nil =>
        (defaultBaseName, listPropertiesFiles(defaultBaseName))
      case n :: Nil =>
        (n, listPropertiesFiles(n))
      case n :: paths =>
        (n, paths.map(new File(_)).toArray)
    }
    val asMessages = true//simpleClassName.endsWith("Messages")
    compileResouces(
      fullClassName,
      packageName,
      simpleClassName,
      inputFiles,
      asMessages,
      error => println("Error: " + error),
      warning => println("Warning: " + warning),
      info => println("Info: " + info)
    )
  }
  def compileResources(
    fullClassName: String,
    packageName: String,
    simpleClassName: String,
    inputFiles: Seq[File],
    asMessages: Boolean,
    errors: String => Unit,
    warnings: String => Unit,
    infos: String => Unit
  ) {
    val fileProps = inputFiles.map(file => (file, read(file))).toMap
    val referenceFileOpt = fileProps.find(_._1.getName.equals(fullClassName + ".properties")).map(_._1)
    
    val referenceKeys = referenceFileOpt.map(f => fileProps(f).keys).getOrElse(
      fileProps.flatMap(_._2.keys).toSet
    ).toIndexedSeq.sorted
    
    for ((file, props) <- fileProps) {
      val missingKeys = referenceKeys.filter(key => !props.contains(key))
      if (!missingKeys.isEmpty) {
        errors(missingKeys.size + " missing keys in file '" + file + "' :")
        missingKeys.foreach(key => println("\t'" + key + "'"))
      }
      val unknownKeys = props.keys.filter(key => !referenceKeys.contains(key))
      if (!missingKeys.isEmpty) {
        warning(unknownKeys.size + " unknown keys in file '" + file + "' :")
        unknownKeys.foreach(key => println("\t'" + key + "'"))
      }
      
    }
    
    val outputFile = simpleClassName + ".scala"
    val out = new PrintStream(outputFile)
    if (Option(packageName).map(_.trim).getOrElse("") != "")
      out.println("package " + packageName)
    out.println("import java.util._")
    out.println("import java.text._")
    out.println("object " + simpleClassName + " {")
    out.println("\tprivate var $resourceBundle: ResourceBundle = _")
    out.println("""
  private var $locale = Locale.getDefault
  private def $loadBundle(loc: Locale) = this synchronized {
    $resourceBundle = ResourceBundle.getBundle(""" + "\"" + fullClassName + "\"" + """, $locale)
  }
  $loadBundle($locale)
  
  def locale = $locale
  def locale_=(loc: Locale) = this synchronized {
    $loadBundle(loc)
    $locale = loc
  }
    """)
    
    def outputExampleJavaDoc(key: String) =
      for (f <- referenceFileOpt; msg <- fileProps(f).get(key))
        out.println("\t/** Reference message : \"" + msg + "\" */")
        
    def outputVal(key: String) = {
      outputExampleJavaDoc(key)
      out.println("\tlazy val " + key + " = $resourceBundle.getString(\"" + key + "\")")
      out.println()
    }
      
    if (asMessages) {
      import java.text._
      for (key <- referenceKeys) {
        val formats = (
          fileProps.flatMap { case (file, props) => 
            for (message <- props.get(key)) yield {
              val fmt = new MessageFormat(message)
              fmt.getFormats.zipWithIndex.map {
                case (argFmt, index) =>
                  (index, argFmt, file)
              }
            }
          }
        ).flatten.groupBy(_._1) // group by index
        val argTypes = for ((index, list) <- formats) yield {
          val types = list.map({ case (_, fmt, file) => 
            (
              fmt match {
                case null =>
                  null
                case f: DecimalFormat if f.isParseIntegerOnly =>
                  "Long"
                case f: DecimalFormat if f.isParseBigDecimal =>
                  "BigDecimal"
                case _: DateFormat =>
                  "java.util.Date"
                case _: MessageFormat =>
                  "String"
                case _: DecimalFormat =>
                  "Double"
                case _ =>
                  "Any"
              },
              file
            )
          }).groupBy(_._1)
          
          val actualTypes = types.filter(_._1 ne null)
          //println("Types for key '" + key + "' = " + types.map(_._1))
          (
            index,
            if (actualTypes.isEmpty)
              "Any"
            else if (actualTypes.size == 1)
              actualTypes.head._1
            else {
              hasError = true
              errors(parameter " + index + " of message '" + key + "' has inconsistent format over the localized messages :")
              for ((_, file) <- actualTypes.flatMap(_._2)) println("\tMessage in file '" + file + "' : \n\t\t'" + fileProps(file)(key) + "'")
              "Any"
            }
          )
        }
        
        if (argTypes.isEmpty)
          outputVal(key)
        else {
          val minIndex = argTypes.keys.min
          if (minIndex > 0)
            warnings("lowest parameter index in key '" + key + "' is " + minIndex + " (might be an error !)")
          val argCount = argTypes.keys.max + 1
          val args = for (i <- 0 until argCount) yield {
            val name = "arg" + i
            (
              name + ": " + argTypes.getOrElse(i, "Any"),
              name + ".asInstanceOf[AnyRef]"
            )
          }
          out.println("\tprivate lazy val " + key + "$fmt = new MessageFormat($resourceBundle.getString(\"" + key + "\"))")
          outputExampleJavaDoc(key)
          out.println("\tdef " + key + "(" + args.map(_._1).mkString(", ") + ") = " + key + "$fmt.format(Array[AnyRef](" + args.map(_._2).mkString(", ") + "))")
          out.println()
        }
      }
    } else {
      referenceKeys.foreach(outputVal)
    }
    out.println("}")
    out.close
    
    infos("wrote " + referenceKeys.size + " keys in source file '" + outputFile + "'")
    if (hasError)
      System.exit(1)
  }
}
