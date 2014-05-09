package network;

import javax.crypto.*;
import javax.crypto.Cipher;
import java.security.Security;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;
import java.security.KeyPair;

/**
 * Created by weeeeeew on 2014-04-29.
 */
public class Crypto {
    public final static String AGREEMENT_ALGORITHM = "DiffieHellman";
    public final static int AGREEMENT_KEY_SIZE = 1024;

    public final static String ENCRYPTION_ALGORITHM = "AES/CBC/PKCS5Padding";
    public final static String SECRET_KEY_ALGORITHM = "AES";
    public final static int SECRET_KEY_SIZE = 128;

    public final static String SIGNATURE_ALGORITHM = "SHA1withDSA";
    public final static String SIGNATURE_KEY_ALGORITHM = "DSA";
    public final static int SIGNATURE_KEY_SIZE = 1024;

    private final static Cipher cipher = initCipher();
    private final static KeyAgreement agreement = initAgreement();
    private final static Signature signer = initSigner();
    private final static KeyPairGenerator agreementKeygen = initKeygen(AGREEMENT_ALGORITHM,AGREEMENT_KEY_SIZE);
    private final static KeyPairGenerator signatureKeygen = initKeygen(SIGNATURE_KEY_ALGORITHM,SIGNATURE_KEY_SIZE);

    static {
        Security.addProvider(new BouncyCastleProvider());
    }

    //TODO javadoc
    public static SealedObject encrypt(Serializable data, SecretKey key) throws InvalidKeyException, IOException, IllegalBlockSizeException {
        if (SECRET_KEY_ALGORITHM.equals(key.getAlgorithm())) {
            synchronized (cipher) {
                cipher.init(Cipher.ENCRYPT_MODE, key);
                return new SealedObject(data, cipher);
            }
        } else {
            throw new InvalidKeyException("Key algorithm must be compatible with "+ENCRYPTION_ALGORITHM);
        }
    }

    /**
     * Decrypts a SealedObject.
     * @param data The SealedObject to be decrypted.
     * @param key The PrivateKey with which the object should be decrypted.
     * @return The decrypted object.
     * @throws InvalidKeyException
     * @throws IllegalBlockSizeException
     * @throws java.io.IOException
     */
    public static Serializable decrypt(SealedObject data, SecretKey key) throws InvalidKeyException, IOException, IllegalBlockSizeException {
        if (data.getAlgorithm().startsWith(key.getAlgorithm())) {
            if (SECRET_KEY_ALGORITHM.equals(key.getAlgorithm())) {
                synchronized (cipher) {
                    cipher.init(Cipher.DECRYPT_MODE, key);
                    try {
                        return (Serializable) data.getObject(cipher);
                    } catch (ClassNotFoundException e) {
                        e.printStackTrace();
                        return null;
                    } catch (BadPaddingException e) {
                        return null;
                        //Decryption failed
                    }
                }
            } else {
                throw new InvalidKeyException("Key algorithm must be compatible with "+ENCRYPTION_ALGORITHM);
            }
        } else {
            throw new InvalidParameterException("Algorithms in SealedObject and SecretKey must match.");
        }
    }

    /**
     * Signs a Serializable object.
     * @param data The object to be signed.
     * @param key The key with which to sign the object.
     * @return A SignedObject.
     * @throws InvalidKeyException
     * @throws IOException
     * @throws SignatureException
     */
    public static SignedObject sign(Serializable data, PrivateKey key) throws InvalidKeyException, IOException, SignatureException {
        synchronized (signer) {
            return new SignedObject(data,key,signer);
        }
    }

    /**
     * Verifies the authenticity of a signed object.
     * @param data The object to be verified.
     * @param key The public key of the (presumed) signer.
     * @return True if the signature was in fact created with the corresponding private key, false otherwise.
     * @throws SignatureException
     * @throws InvalidKeyException
     */
    public static boolean verify(SignedObject data, PublicKey key) throws SignatureException, InvalidKeyException {
        synchronized (signer) {
            return data.verify(key,signer);
        }
    }

    /**
     * Generates a keypair for secure secret key agreement purposes.
     * @return The keypair.
     */
    public static KeyPair generateAgreementKeyPair() {
        synchronized (agreementKeygen) {
            return agreementKeygen.generateKeyPair();
        }
    }

    /**
     * Generates a key pair for signing and verification purposes.
     * @return The key pair.
     */
    public static KeyPair generateSignatureKeyPair() {
        synchronized (signatureKeygen) {
            return signatureKeygen.generateKeyPair();
        }
    }

    /**
     * Securely agrees on a secret key for two peers.
     * @param myKey This peer's private Diffie-Hellman key.
     * @param otherKey The other peer's public Diffie-Hellman key.
     * @return The secret key, which will be the same for both peers.
     * @throws InvalidKeyException
     */
    public static SecretKey generateSecretKey(PrivateKey myKey, PublicKey otherKey) throws InvalidKeyException {
        if (AGREEMENT_ALGORITHM.equals(myKey.getAlgorithm()) && AGREEMENT_ALGORITHM.equals(otherKey.getAlgorithm())) {
            synchronized (agreement) {
                agreement.init(myKey);
                agreement.doPhase(otherKey, true);

                try {
                    return agreement.generateSecret(SECRET_KEY_ALGORITHM);
                } catch (NoSuchAlgorithmException e) {
                    e.printStackTrace();
                    //The Java platform is defective.
                    return null;
                }
            }
        } else {
            throw new InvalidKeyException("Key algorithms must be compatible with "+ AGREEMENT_ALGORITHM);
        }
    }

    private static Cipher initCipher() {
        try {
            return Cipher.getInstance(ENCRYPTION_ALGORITHM);
        } catch (NoSuchAlgorithmException|NoSuchPaddingException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }

    private static KeyAgreement initAgreement() {
        try {
            return KeyAgreement.getInstance(AGREEMENT_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required KeyAgreement algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/KeyAgreement.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }

    private static Signature initSigner() {
        try {
            return Signature.getInstance(SIGNATURE_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required Signature algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/java/security/Signature.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }

    private static KeyPairGenerator initKeygen(String agreementAlgorithm, int keySize) {
        try {
            KeyPairGenerator keygen = KeyPairGenerator.getInstance(agreementAlgorithm);
            keygen.initialize(keySize);
            return keygen;
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required KeyPairGenerator algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/java/security/KeyPairGenerator.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }
}
