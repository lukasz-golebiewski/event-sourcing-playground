package esp.akka.persistence

import esp.PendingIfUnimplemented
import esp.model.ApiTest

class AkkaPersistenceApiTest extends ApiTest with AkkaPersistenceApi with PendingIfUnimplemented
