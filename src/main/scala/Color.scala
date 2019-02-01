object Color {
	def printRed(s: String):    Unit = print(s"\033[31m$s\033[m")
	def printGreen(s: String):  Unit = print(s"\033[32m$s\033[m")
	def printYellow(s: String): Unit = print(s"\033[33m$s\033[m")
	def printPurple(s: String): Unit = print(s"\033[35m$s\033[m")
	def printBlue(s: String):   Unit = print(s"\033[36m$s\033[m")
}
