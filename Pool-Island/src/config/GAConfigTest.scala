package config

import com.google.gson.Gson
import java.io.FileReader
import java.io.File

import config.GAConfig._

object GAConfigTest extends App {

		val g = new GAConfig();
		g.setChromosomeSize(34)
		
		//GAConfig.saveToJSON(g, "config.json")
		println(GAConfig.loadFromJSON("config.json"))
}