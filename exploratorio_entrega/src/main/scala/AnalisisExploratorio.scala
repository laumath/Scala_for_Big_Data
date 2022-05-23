import java.lang.Math.round

object AnalisisExploratorio extends Analizador {

  // ejercicio-1:
  // Popula la variable dataset con el resultado de la función loadDataset de Utilidades.
  // Ten en cuenta que se carga el csv completo, incluyendo las cabeceras, asegúrate de omitirlas (la primera fila)
  val dataset = Utilidades.loadDataset(Utilidades.filePath).drop(1)

  // Implementa la función
  // ejercicio-2:
  // Número total de registros en el dataset.
  def totalDeRegistros(c: Seq[Contribuyente]): Int = dataset.size


  // Implementa la función
  // ejercicio-3:
  // Calcular la media de edad de todos los contribuyentes
  def calculaEdadMedia(c: Seq[Contribuyente]): Double = {
    val sum_age = dataset.map(_.age).sum
    round(sum_age / totalDeRegistros(dataset).toFloat * 100.0)/100.0
  }

  // Implementa la función
  // ejercicio-4:
  // Calcular la media de edad de todos los contribuyentes que nunca se han casado.
  // hint: marital-status = Never-Married
  def calculaEdadMediaNeverMarried(c: Seq[Contribuyente]): Double = {
    val tnever_married = dataset.filter(_.maritalStatus.contains("Never-married")).size
    val s_age_never_married = dataset.filter(_.maritalStatus.contains("Never-married")).filter(_.age > 0).map(_.age).sum
    round(s_age_never_married / tnever_married.toFloat *100.0)/100.0
  }


  // Implementa la función
  // ejercicio-5:
  // Descubrir de cuántos países distintos provienen los contribuyentes
  def paisesOrigenUnicos(c: Seq[Contribuyente]): Int = {
    val paises_distintos = dataset.map(_.nativeCountry).distinct.size
    paises_distintos
  }



  def paises(c: Seq[Contribuyente]): Seq[String]  = {
    val paises_distintos = dataset.map(_.nativeCountry).distinct
    paises_distintos
  }


  // Implementa la función
  // ejercicio-6:
  // De todos los contribuyentes, ¿cómo se distribuye por género?. Devuelve el porcentaje de hombres
  // y el de mujeres, en ese orden, (porcentajeDeHombres, porcentajeDeMujeres)
  def distribucionPorGeneros(c: Seq[Contribuyente]): (Double, Double) = {
    val Female = dataset.filter(_.sex.contains("Female")).size * 100.0 / totalDeRegistros(dataset)
    val Male = dataset.filter(_.sex.contains("Male")).size * 100.0 / totalDeRegistros(dataset)
    (Male,Female)
  }

  // Implementa la función
  // ejercicio-7:
  // Encuentra el tipo de trabajo (workclass) mejor remunerado. El trabajo mejor remunerado es aquel trabajo donde el
  // porcentaje de los contribuyentes que perciben ingresos (income) superiores a ">50K" es mayor que los contribuyentes
  // cuyos ingresos son "<50K".
  def trabajoMejorRemunerado(c: Seq[Contribuyente]): String  = {
    val income = dataset.groupBy(_.workclass).map(a => (a._1, a._2.filter(_.income == ">50K").map(_.income).size.toDouble/a._2.size))
    val out_income = income.toSeq.sortBy(_._2)
    out_income.last._1
  }


  // Implementa la función
  // ejercicio-8:
  // Cuál es la media de años de educación (education-num) de aquellos contribuyentes cuyo país de origen no es
  // United-States


  def aniosEstudiosMedio(c: Seq[Contribuyente]): Double = {
    val cont_not_US = dataset.filter(_.nativeCountry !=  "United-States").map(_.educationNum).sum
    val sum_edu_num = dataset.filter(_.nativeCountry !=  "United-States").size
    round(cont_not_US/sum_edu_num.toDouble * 100.0)/100.0
  }


  println(s" -> Dataset tiene un total de registros: ${totalDeRegistros(c = dataset)}")
  println(s" -> En el dataset, los contribuyentes tienen una edad media: ${calculaEdadMedia(c = dataset)}")
  println(s" -> En el dataset, los contribuyentes tienen una edad media (sin contar aquellos con age = 0): ${calculaEdadMediaNeverMarried(c = dataset)}")
  println(s" -> Los contribuyentes proviende de distintos países como: ${paises(c = dataset).mkString(",")}")
  println(s" -> Cantidad de paises distintos de donde provienen los contribuyentes: ${paisesOrigenUnicos(c = dataset)}")
  println(s" -> Los contribuyentes se distribuyen en (hombres - mujeres): ${distribucionPorGeneros(c = dataset)}")
  println(s" -> El tipo de trabajo mejor remunerado en el dataset es: ${trabajoMejorRemunerado(c = dataset)}")
  println(s" -> La media de años de estudio de los contribuyenes de origen distinto a United States es: ${aniosEstudiosMedio(c = dataset)}")


  // ejercicio-12
  // llama a la función imprimeContribuyentes pasándole los primeros 5 contribuyentes del dataset.
  //Esta es la funcionalidad del apartado, dado que, depende del anterior.
  println(imprimeContribuyentes(dataset.take(5)))

}
