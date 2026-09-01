package okay.security

/**
 * The Crypto seam over JCA — every primitive the JDK already ships,
 * which is why stage 0 costs zero dependencies. The node:crypto
 * given is the security-node stage.
 */
given Crypto = new Crypto:

  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val mac = javax.crypto.Mac.getInstance("HmacSHA256")
    mac.init(javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(data)

  def sha256(data: Array[Byte]): Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-256").digest(data)

  def signRsaSha256(key: java.security.PrivateKey, data: Array[Byte]): Array[Byte] =
    val s = java.security.Signature.getInstance("SHA256withRSA")
    s.initSign(key)
    s.update(data)
    s.sign()

  def verifyRsaSha256(key: java.security.PublicKey, data: Array[Byte],
                      sig: Array[Byte]): Boolean =
    try
      val s = java.security.Signature.getInstance("SHA256withRSA")
      s.initVerify(key)
      s.update(data)
      s.verify(sig)
    catch case _: Exception => false   // a mangled signature is a refusal, not a fault

  def pbkdf2(password: Array[Char], salt: Array[Byte],
             iterations: Int, bits: Int): Array[Byte] =
    val spec = javax.crypto.spec.PBEKeySpec(password, salt, iterations, bits)
    try javax.crypto.SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
      .generateSecret(spec).getEncoded
    finally spec.clearPassword()

  def randomBytes(n: Int): Array[Byte] =
    val bytes = new Array[Byte](n)
    java.security.SecureRandom().nextBytes(bytes)
    bytes
