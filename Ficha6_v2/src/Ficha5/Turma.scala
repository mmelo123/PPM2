package Ficha5

import Ficha5.RegimeOPT.RegimeOPT
import Ficha5.Turma._


case class Turma(id: String, alunos: Alunos){

  //a
  def trabs(): Alunos = Turma.trabs(this)
  //b
  def searchStudent(n: Numero): Option[Aluno] = Turma.searchStudent(alunos, n)
  //e
  def changeNP(n:Numero,np:NP): Turma = Turma.changeNP(n,np,this)
}

object Turma {

  type Nome = String
  type Numero = Int
  type NT = Option[Float]
  type NP = Option[Float]
  type Regime = RegimeOPT
  type Aluno = (Numero, Nome, Regime, NT, NP)
  type Alunos = List[Aluno]


  def trabs(t:Turma) : Alunos = { t.alunos filter (x => x._3 == RegimeOPT.TrabEstud) }

  def trabs1(t: Turma): Alunos = {
    def aux(l: Alunos): Alunos = l match {
      case Nil => Nil
      case x::xs => if(x._3 == RegimeOPT.TrabEstud) x::aux(xs) else aux(xs)
    }
    aux(t.alunos)
  }

  def searchStudent(t: Alunos, n: Numero) : Option[Aluno] = {
    t match {
      case Nil => None
      case x::xs => if(x._1 == n) Some(x) else searchStudent(xs, n)
    }
  }

  def changeNP(n: Numero, np: NP, t: Turma): Turma = {
    val index = t.alunos.indexWhere(x => { x._1 == n } )
    if(index != -1)
    {
      val al = t.alunos.apply(index)
      val al1 = (al._1, al._2, al._3, al._4, np)
      val t1 = new Turma(t.id, t.alunos.updated(index, al1))
      t1
    }
    else t
  }

  def changeNP1(n: Numero, np: NP, t: Turma): Turma = {
    val al = searchStudent(t.alunos, n)

    if (al != None) {
      val al1 = (al.get._1, al.get._2, al.get._3, al.get._4, np)
      val index = t.alunos.indexWhere(x => { x._1 == n } )
      val t1 = new Turma(t.id, t.alunos.updated(index, al1))
      t1
    }
    else t
  }

}



