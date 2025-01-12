package com.wissenstein.test.discipline

import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline

trait FreeSpecDiscipline extends Discipline, AnyFreeSpecLike:
  self: Configuration =>

  override def checkAll(
    name: String,
    ruleSet: Laws#RuleSet
  )(using
    config: PropertyCheckConfiguration,
    prettifier: Prettifier,
    pos: Position
  ): Unit =
    name - {
      given conf: Checkers.PropertyCheckConfiguration = convertConfiguration(config)

      for (id, prop) <- ruleSet.all.properties do
        id in {
          Checkers.check(prop)
        }
    }
