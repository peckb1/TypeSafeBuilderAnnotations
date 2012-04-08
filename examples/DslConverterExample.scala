import org.cb.dslconverter.annotations.DslConvertable
import org.cb.dslconverter.annotations.Called

@DslConvertable
case class DslConverterExample(
		@Called("atTime") time: Double
) {
  def temp(): Int = {
    3
  }
}

case class RegularCaseExample(
    time: Double
)