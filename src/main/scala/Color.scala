object Color {
	def printRed(s: String):    Unit = print(s"\033[31;1m$s\033[m")
	def printGreen(s: String):  Unit = print(s"\033[32;1m$s\033[m")
	def printYellow(s: String): Unit = print(s"\033[33;1m$s\033[m")
	def printPurple(s: String): Unit = print(s"\033[35;1m$s\033[m")
	def printBlue(s: String):   Unit = print(s"\033[36;1m$s\033[m")
}
