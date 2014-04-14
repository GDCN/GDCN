package network;

import javax.crypto.*;
import javax.crypto.interfaces.DHPrivateKey;
import javax.crypto.interfaces.DHPublicKey;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;

/**
 * Created by weeeeeew on 2014-04-07.
 */
public class Crypto {
    public final static String ASYMMETRIC_ALGORITHM = "RSA/ECB/PKCS1Padding";
    public final static String SYMMETRIC_ALGORITHM = "AES/ECB/PKCS5Padding";
    public final static String SIGN_ALGORITHM = "SHA256withRSA";
    public final static String EXCHANGE_ALGORITHM = "DiffieHellman";

    private final Cipher asymmetric;
    private final Cipher symmetric;
    private final Signature signer;
    private final KeyAgreement agreement;

    public Crypto(DHPrivateKey myKey) {
        asymmetric = initAsymmetric();
        symmetric = initSymmetric();
        signer = initSigner();
        agreement = initAgreement(myKey);
    }

    /**
     * Encrypts a Serializable object.
     * @param data The object to be encrypted.
     * @param key The PublicKey it should be encrypted with.
     * @return A SealedObject.
     * @throws InvalidKeyException
     * @throws IOException
     */
    public SealedObject encrypt(Serializable data, PublicKey key) throws Exception {
        if (ASYMMETRIC_ALGORITHM.startsWith(key.getAlgorithm())) {
            synchronized (asymmetric) {
                asymmetric.init(Cipher.ENCRYPT_MODE, key);
                return new SealedObject(data, asymmetric);
            }
        } else {
            throw new InvalidKeyException("Key algorithm must be compatible with "+ASYMMETRIC_ALGORITHM);
        }
    }

    public SealedObject encrypt(Serializable data, SecretKey key) throws Exception {
        if (SYMMETRIC_ALGORITHM.startsWith(key.getAlgorithm())) {
            synchronized (symmetric) {
                symmetric.init(Cipher.ENCRYPT_MODE, key);
                return new SealedObject(data, symmetric);
            }
        } else {
            throw new InvalidKeyException("Key algorithm must be compatible with "+SYMMETRIC_ALGORITHM);
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
     * @throws IOException
     */
    public Serializable decrypt(SealedObject data, PrivateKey key) throws InvalidKeyException, IllegalBlockSizeException, IOException, ClassNotFoundException, BadPaddingException {
        if (data.getAlgorithm().startsWith(key.getAlgorithm())) {
            if (ASYMMETRIC_ALGORITHM.startsWith(key.getAlgorithm())) {
                synchronized (asymmetric) {
                    asymmetric.init(Cipher.DECRYPT_MODE, key);
                    return (Serializable) data.getObject(asymmetric);
                }
            } else {
                throw new InvalidKeyException("Key algorithm must be compatible with "+ASYMMETRIC_ALGORITHM);
            }
        } else {
            throw new InvalidParameterException("Algorithms in SealedObject and PrivateKey must match.");
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
     * @throws IOException
     */
    public Serializable decrypt(SealedObject data, SecretKey key) throws InvalidKeyException, IllegalBlockSizeException, IOException, ClassNotFoundException, BadPaddingException {
        if (data.getAlgorithm().startsWith(key.getAlgorithm())) {
            if (SYMMETRIC_ALGORITHM.startsWith(key.getAlgorithm())) {
                synchronized (symmetric) {
                    symmetric.init(Cipher.DECRYPT_MODE, key);
                    return (Serializable) data.getObject(symmetric);
                }
            } else {
                throw new InvalidKeyException("Key algorithm must be compatible with "+SYMMETRIC_ALGORITHM);
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
    public SignedObject sign(Serializable data, PrivateKey key) throws InvalidKeyException, IOException, SignatureException {
        synchronized (signer) {
            return new SignedObject(data,key,signer);
        }
    }

    /**
     * Verifies the authenticity of a SignedObject.
     * @param data The SignedObject to be verified.
     * @param key The PublicKey it should be checked against.
     * @return Boolean of the result.
     * @throws SignatureException
     * @throws InvalidKeyException
     */
    public boolean verify(SignedObject data, PublicKey key) throws SignatureException, InvalidKeyException {
        synchronized (signer) {
            return data.verify(key,signer);
        }
    }

    /**
     * Signs and encrypts a Serializable object.
     * @param data The object to be signed and encrypted.
     * @param signKey The PrivateKey with which to sign the object.
     * @param encryptionKey The PublicKey with which to encrypt the object.
     * @return encrypted message
     */
    public SealedObject signAndEncrypt(Serializable data, PrivateKey signKey, PublicKey encryptionKey) throws Exception {
        SignedObject signedObject = sign(data, signKey);
        return encrypt(signedObject, encryptionKey);
    }

    /**
     * Decrypts and verifies SealedObject which must contain a SignedObject.
     * @param data The SealedObject to be decrypted and verified (must contain a SignedObject).
     * @param decryptKey The PrivateKey with which to decrypt the object.
     * @param verifyKey The PublicKey with which to verify the object.
     * @return The decrypted object.
     * @throws Exception
     */
    public Serializable decryptAndVerify(SealedObject data, PrivateKey decryptKey, PublicKey verifyKey) throws Exception {
        Object decrypted = decrypt(data,decryptKey);
        SignedObject signedData;

        if (decrypted instanceof SignedObject) {
            signedData = (SignedObject) decrypted;
        } else {
            throw new InvalidParameterException("The encrypted object was not signed");
        }

        if (verify(signedData,verifyKey)) {
            return (Serializable) signedData.getObject();
        } else {
            System.out.println("in Crypto.decryptAndVerify: ERROR! Signature did not match! Object: "+signedData.getObject());
            return null;
        }
    }

    public SecretKey generateSecretKey(DHPublicKey otherKey) throws Exception {
        synchronized (agreement) {
            agreement.doPhase(otherKey, true);

            return agreement.generateSecret("AES");
        }
    }

    private static Cipher initAsymmetric() {
        try {
            return Cipher.getInstance(ASYMMETRIC_ALGORITHM);
        } catch (NoSuchAlgorithmException|NoSuchPaddingException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }

    private static Cipher initSymmetric() {
        try {
            return Cipher.getInstance(SYMMETRIC_ALGORITHM);
        } catch (NoSuchAlgorithmException|NoSuchPaddingException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
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

    private static KeyAgreement initAgreement(DHPrivateKey myKey) {
        try {
            KeyAgreement ka = KeyAgreement.getInstance(EXCHANGE_ALGORITHM);
            ka.init(myKey);

            return ka;
        } catch (NoSuchAlgorithmException|InvalidKeyException e) {
            //The Java platform is defective, it does not support all required KeyAgreement algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/KeyAgreement.html
            e.printStackTrace();
            throw new ExceptionInInitializerError(e);
        }
    }
}
