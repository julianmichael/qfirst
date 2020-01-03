package freelog

/** Contract: branch must block as well. */
trait EphemeralTreeLogger[Msg, F[_]] extends EphemeralLogger[Msg, F] with TreeLogger[Msg, F]
