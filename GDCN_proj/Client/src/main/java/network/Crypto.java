package network;

import javax.crypto.*;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;

/**
 * Created by weeeeeew on 2014-04-07.
 */
public class Crypto {
    public final static String ENCRYPT_ALGORITHM = "RSA/ECB/PKCS1Padding";
    public final static String SIGN_ALGORITHM = "SHA256withRSA";
    private final static Cipher cipher = initCipher();
    private final static Signature signer = initSigner();


    /**
     * Encrypts a Serializable object.
     * @param data The object to be encrypted.
     * @param key The PublicKey it should be encrypted with.
     * @return A SealedObject.
     * @throws InvalidKeyException
     * @throws IOException
     */
    public static SealedObject encrypt(Serializable data, PublicKey key) throws InvalidKeyException, IOException {
        synchronized (cipher) {
            cipher.init(Cipher.ENCRYPT_MODE, key);
            try {
                return new SealedObject(data,cipher);
            } catch (IllegalBlockSizeException e) {
                //We should never get here, because we don't use block ciphers...
                e.printStackTrace();
                return null;
            }
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
    public static Serializable decrypt(SealedObject data, PrivateKey key) throws InvalidKeyException, ClassNotFoundException, BadPaddingException, IllegalBlockSizeException, IOException {
        synchronized (cipher) {
            cipher.init(Cipher.DECRYPT_MODE,key);

            return (Serializable) data.getObject(cipher);
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
     * Verifies the authenticity of a SignedObject.
     * @param data The SignedObject to be verified.
     * @param key The PublicKey it should be checked against.
     * @return Boolean of the result.
     * @throws SignatureException
     * @throws InvalidKeyException
     */
    public static boolean verify(SignedObject data, PublicKey key) throws SignatureException, InvalidKeyException {
        synchronized (signer) {
            return data.verify(key,signer);
        }
    }

    /**
     * Signs and encrypts a Serializable object.
     * @param data The object to be signed and encrypted.
     * @param myKey The PrivateKey with which to sign the object.
     * @param otherKey The PublicKey with which to encrypt the object.
     * @return encrypted message
     */
    public static SealedObject signAndEncrypt(Serializable data, PrivateKey myKey, PublicKey otherKey) throws IOException, InvalidKeyException, SignatureException {
        SignedObject signedObject = sign(data,myKey);
        return encrypt(signedObject, otherKey);
    }

    /**
     * Decrypts and verifies SealedObject which must contain a SignedObject.
     * @param data The SealedObject to be decrypted and verified (must contain a SignedObject).
     * @param myKey The PrivateKey with which to decrypt the object.
     * @param otherKey The PublicKey with which to verify the object.
     * @return The decrypted object.
     * @throws Exception
     */
    public static Serializable decryptAndVerify(SealedObject data, PrivateKey myKey, PublicKey otherKey) throws Exception {
        Object decrypted = decrypt(data,myKey);
        SignedObject signedData;

        if (decrypted.getClass() == SignedObject.class) {
            signedData = (SignedObject) decrypted;
        } else {
            throw new Exception("Wrong class!");
        }

        if (Crypto.verify(signedData,otherKey)) {
            return (Serializable) signedData.getObject();
        } else {
            throw new Exception("ERROR! ERROR! Signature did not match.");
        }
    }

    private static Cipher initCipher() {
        try {
            return Cipher.getInstance(ENCRYPT_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
            e.printStackTrace();
            throw new ExceptionInInitializerError();
        } catch (NoSuchPaddingException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
            e.printStackTrace();
            throw new ExceptionInInitializerError();
        }
    }

    private static Signature initSigner() {
        try {
            return Signature.getInstance(SIGN_ALGORITHM);
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required Signature algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/java/security/Signature.html
            e.printStackTrace();
            throw new ExceptionInInitializerError();
        }
    }
}
