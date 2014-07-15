package prog_synth_xpath

import scala.util.Random
import java.io.PrintWriter
import scala.sys.process._
import scala.collection.mutable._

object xml_generator {
    val rounds = 1 // since the memory is limited, so we need to split the file and generate part of it in each round.
	val num_bookstore = 50;
	val num_staff = 100;
	val num_book = 100;
	val rseed = 100;
	val gen = new Random(rseed)
	
	def generator = {
	  println("start to print")
	  val out = new PrintWriter(s"${Data.root}/xml/bookstore.xml")
	  out.println("<?xml version=\"1.0\"?>\n<!DOCTYPE bookstore SYSTEM \"bookstore.dtd\">\n\n")
	  out.println(s"<source>\n")
	  (0 until rounds).map(_ => out.println((new source).myprint))
	  out.println(s"</source>\n")
	  out.close
	  println("Done")
	}
	
	def i(max: Int) = {
	   gen.nextInt(max) 
	}
	  
	  def s(len: Int) = {
	    gen.alphanumeric.take(len).mkString 
	}
	  
	def ai(max: Int) = {
	    "\"" + gen.nextInt(max) + "\""
	}
	  
	  def as(len: Int) = {
	    "\"" + gen.alphanumeric.take(len).mkString + "\""
	}
}

class source {
  val bookstores = (0 until xml_generator.num_bookstore).map(_ => new bookstore)
  def myprint : String = {
	  bookstores.toVector.map(_.myprint).mkString("") 
  }  
}

class bookstore {
  val name = xml_generator.as(10)
  val location = xml_generator.as(20)
  val books = (0 until xml_generator.num_book).map(_ => new book)
  val staffs = (0 until xml_generator.num_staff).map(_ => new staff)
  def myprint : String = {
	  s"  <bookstore name=$name location=$location>\n" + books.map(_.myprint).mkString("") + staffs.map(_.myprint).mkString("") + s"  </bookstore>\n"
  } 
}

class book {
  val id = xml_generator.ai(100)
  val title = xml_generator.as(20)
  val bookT = new booktype
  val authorN = new author
  
  def myprint : String = {
	  s"    <book bid=$id title=$title>\n" + bookT.myprint + authorN.myprint + s"    </book>\n"
  } 
}

class booktype {
  val text = xml_generator.s(20)
  def myprint : String = {
    s"      <booktype>\n        $text\n      </booktype>\n"
  }
}

class author {
  val name = xml_generator.s(10)
  def myprint : String = {
    s"      <author>\n        $name\n      </author>\n"
  }
}

class staff {
  val id = xml_generator.ai(100)
  val name = xml_generator.as(20)
  val roleN = new role
  
  def myprint : String = {
	  s"    <staff sid=$id name=$name>\n" + roleN.myprint + s"    </staff>\n"
  } 
}

class role {
  val name = xml_generator.s(10)
  def myprint : String = {
    s"      <role>\n         $name\n      </role>\n"
  }
}

