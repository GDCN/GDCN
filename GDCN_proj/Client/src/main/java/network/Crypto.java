package network;

import javax.crypto.*;
import javax.crypto.Cipher;
import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;
import java.security.KeyPair;

/**
 * Created by weeeeeew on 2014-04-29.
 */
public class Crypto {
    public final static String EXCHANGE_ALGORITHM = "DiffieHellman";
    public final static String ENCRYPTION_ALGORITHM = "AES/ECB/PKCS5Padding";
    public final static String SIGN_ALGORITHM = "SHA256withRSA";

    private final static Cipher cipher;
    private final static KeyAgreement agreement;
    private final static Signature signer;

    static {
        cipher = initCipher();
        agreement = initAgreement();
        signer = initSigner();
    }

    public static SealedObject encrypt(Serializable data, SecretKey key) throws InvalidKeyException, IOException, IllegalBlockSizeException {
        if (ENCRYPTION_ALGORITHM.startsWith(key.getAlgorithm())) {
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
     * @throws ClassNotFoundException
     * @throws BadPaddingException
     * @throws IllegalBlockSizeException
     * @throws java.io.IOException
     */
    public static Serializable decrypt(SealedObject data, SecretKey key) throws InvalidKeyException, BadPaddingException, IOException, IllegalBlockSizeException {
        if (data.getAlgorithm().startsWith(key.getAlgorithm())) {
            if (ENCRYPTION_ALGORITHM.startsWith(key.getAlgorithm())) {
                synchronized (cipher) {
                    cipher.init(Cipher.DECRYPT_MODE, key);
                    try {
                        return (Serializable) data.getObject(cipher);
                    } catch (ClassNotFoundException e) {
                        e.printStackTrace();
                        return null;
                        //This should never happen, as it is impossible to encrypt a non-serializable object
                    }
                }
            } else {
                throw new InvalidKeyException("Key algorithm must be compatible with "+ENCRYPTION_ALGORITHM);
            }
        } else {
            throw new InvalidParameterException("Algorithms in SealedObject and SecretKey must match.");
        }
    }

    public static SecretKey generateSecretKey(PrivateKey myKey, PublicKey otherKey) throws InvalidKeyException {
        if (myKey instanceof DHPrivateKey && otherKey instanceof DHPublicKey) {
            synchronized (agreement) {
                agreement.init(myKey);
                agreement.doPhase(otherKey, true);

                try {
                    return agreement.generateSecret("AES/ECB/PKCS5Padding");
                } catch (NoSuchAlgorithmException e) {
                    e.printStackTrace();
                    //The Java platform is defective.
                    return null;
                }
            }
        } else {
            throw new InvalidKeyException("Key algorithms must be compatible with "+EXCHANGE_ALGORITHM);
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

    public static boolean verify(SignedObject data, PublicKey key) {
        synchronized (signer) {
            return data.verify(key,signer);
        }
    }

    public static KeyPair generateDHKeyPair() {
        try {
            return KeyPairGenerator.getInstance(Crypto.EXCHANGE_ALGORITHM).generateKeyPair();
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required KeyAgreement algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/KeyAgreement.html
            e.printStackTrace();
            return null;
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
            return KeyAgreement.getInstance(EXCHANGE_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required KeyAgreement algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/KeyAgreement.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }

    private static Signature initSigner() {
        try {
            return Signature.getInstance(SIGN_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required Signature algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/java/security/Signature.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }
}
