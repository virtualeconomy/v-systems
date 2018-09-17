package vee.settings

import scala.concurrent.duration._

import com.wavesplatform.settings.Constants
import com.wavesplatform.state2.ByteStr


case class GenesisTransactionSettings(recipient: String, amount: Long, slotId: Int)

case class GenesisSettings(
  blockTimestamp: Long,
  timestamp: Long,
  initialBalance: Long,
  signature: Option[ByteStr],
  transactions: Seq[GenesisTransactionSettings],
  initialMintTime: Long,
  averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(1537164018783517311L, 1537164018783517311L, 514285800000000000L,
    ByteStr.decodeBase58("45jkYxYcTrLkgwVdaQzsnzgeGz4Ebogd28napZ5UNQuZw5XD2SLjqFyEnfNCFaGEiRkYHVVyyK1QwQxjia86k2Yo").toOption,
    List(
      GenesisTransactionSettings("ARMxPV8puShq49KzPoQb5XGttxghvLdMFg1",36000006000000000L,-1),
      GenesisTransactionSettings("ARFHdvqddPNfMXNt9fgH7Lw1aGRgL29wTKY",15428574000000000L,-1),
      GenesisTransactionSettings("ARQSe9A3DLryBqN4Y1NV1PhERnuWaDLheza",25097147000000000L,-1),
      GenesisTransactionSettings("ARHY8cocXeGnH3q4bkLpU56sCg28fF5hSWV",36000006000000000L,-1),
      GenesisTransactionSettings("ARCry9n7JvHF8rooXiEMhesAHP8zv3sBLDr",36000006000000000L,-1),
      GenesisTransactionSettings("AR7M83ZypZc5WRzdZHjhULnQ7xzoMM8udJt",12342859200000000L,-1),
      GenesisTransactionSettings("ARPdpZ9PT5kDbi2U6ZUgxoFwJzFchz7Fcsw",36000006000000000L,-1),
      GenesisTransactionSettings("ARCK84TwkqzQEksj8pQTxbsCuRY9YGsAbSU",36000006000000000L,-1),
      GenesisTransactionSettings("ARKbFz4N3f8Rgonf1Afd2aix2QtkCUjnWqW",39085720800000000L,-1),
      GenesisTransactionSettings("ARRo5K1rfLk9wjXhUjAqxdZjK7LTjx7Y7ki",36000006000000000L,-1),
      GenesisTransactionSettings("ARBLnj5P9XNspGRGj8UjS1Fny3pkvxJUqJV",36000006000000000L,-1),
      GenesisTransactionSettings("AR5c7ibqGhkyjzYu143E171TiFdmPFHofKb",36000006000000000L,-1),
      GenesisTransactionSettings("ARJukkvk47P9JKpdpjbvPQFr8Lv4D8Qsqnn",36000006000000000L,-1),
      GenesisTransactionSettings("ARBD7fqPGPq5stYKyk3EbZe1w1FDYhh2KnZ",36000006000000000L,-1),
      GenesisTransactionSettings("ARH6GEvr8FV12HN4rZ969K885sukp5qdNJY",36000006000000000L,-1),
      GenesisTransactionSettings("AR2pa1RW5jvKV4b7PNnZG7Az6emz5WtiE4r",26331433000000000L,-1),
      GenesisTransactionSettings("ARQcQXvUVeZ55cV7VWBzvTZCfGLNcQQ4BSm",0L,0),
      GenesisTransactionSettings("ARPbddZcV3gpXT4WBxVP44HAgroqeQw38kt",0L,4),
      GenesisTransactionSettings("AR73vwfW1sNALXCZTqbypNuEEbPTKccERok",0L,12),
      GenesisTransactionSettings("AR3yL8ALb4oLYqUruLMmi6DYcNiCrCCXXrH",0L,16),
      GenesisTransactionSettings("ARL8X83xFfU9aHXQoP6HW5kPzrCurLjoCT1",0L,24),
      GenesisTransactionSettings("AR2hWH9sugKjdArn7A9yYkUWQdGHjefcJq9",0L,28),
      GenesisTransactionSettings("AR8QbPBWx9x2yeWCWXdJVxftb7RQ6hqwAiU",0L,36),
      GenesisTransactionSettings("AR4SSjr2tuZxSmJpfwHCbxWEp2xQERh4JWL",0L,40),
      GenesisTransactionSettings("AR8n5DjETjUKFApuJNRhkWsTbWPeSYQorBS",0L,48),
      GenesisTransactionSettings("AR7mJnfqmEw5pwCyWkJRqdbcVhGhaycLaVs",0L,52)),
    1537164018000000000L, 60.seconds)

  val TESTNET = GenesisSettings(1535356447650226656L, 1535356447650226656L, Constants.UnitsInVee * Constants.TotalVee,
    ByteStr.decodeBase58("5n4ewwZh9F4MMpSvtdxLCu5MUKnhEyUth2w3zEfpuiX3vwS1STNCdi51fmowJuLT1CfFg1DuodSvxwBZDANvGNej").toOption,
    List(
      GenesisTransactionSettings("ATxpELPa3yhE5h4XELxtPrW9TfXPrmYE7ze", (Constants.UnitsInVee * Constants.TotalVee * 0.30).toLong, 0),
      GenesisTransactionSettings("ATtRykARbyJS1RwNsA6Rn1Um3S7FuVSovHK", (Constants.UnitsInVee * Constants.TotalVee * 0.20).toLong, 1),
      GenesisTransactionSettings("ATtchuwHVQmNTsRA8ba19juGK9m1gNsUS1V", (Constants.UnitsInVee * Constants.TotalVee * 0.15).toLong, 2),
      GenesisTransactionSettings("AU4AoB2WzeXiJvgDhCZmr6B7uDqAzGymG3L", (Constants.UnitsInVee * Constants.TotalVee * 0.05).toLong, 3),
      GenesisTransactionSettings("AUBHchRBY4mVNktgCgJdGNcYbwvmzPKgBgN", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 4),
      GenesisTransactionSettings("AU6qstXoazCHDK5dmuCqEnnTWgTqRugHwzm", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 5),
      GenesisTransactionSettings("AU9HYFXuPZPbFVw8vmp7mFmHb7qiaMmgEYE", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 6),
      GenesisTransactionSettings("AUBLPMpHVV74fHQD8D6KosA76nusw4FqRr1", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 7),
      GenesisTransactionSettings("AUBbpPbymsrM8QiXqS3NU7CrD1vy1EyonCa", (Constants.UnitsInVee * Constants.TotalVee * 0.04).toLong, 8),
      GenesisTransactionSettings("AU7nJLcT1mThXGTT1KDkoAtfPzc82Sgay1V", (Constants.UnitsInVee * Constants.TotalVee * 0.02).toLong, 9)),
    1535356440000000000L, 60.seconds)
}
