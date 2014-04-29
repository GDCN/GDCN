package network;

import com.jcraft.jsch.*;

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
public class Crypto2 {
    public final static String EXCHANGE_ALGORITHM = "DiffieHellman";
    public final static String ENCRYPTION_ALGORITHM = "AES/ECB/PKCS5Padding";

    private final static Cipher cipher;
    private final static KeyAgreement agreement;

    static {
        cipher = initCipher();
        agreement = initAgreement();
    }

    public static SealedObject encrypt(Serializable data, SecretKey key) throws Exception {
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
    public static Serializable decrypt(SealedObject data, SecretKey key) throws InvalidKeyException, IllegalBlockSizeException, IOException, ClassNotFoundException, BadPaddingException {
        if (data.getAlgorithm().startsWith(key.getAlgorithm())) {
            if (ENCRYPTION_ALGORITHM.startsWith(key.getAlgorithm())) {
                synchronized (cipher) {
                    cipher.init(Cipher.DECRYPT_MODE, key);
                    return (Serializable) data.getObject(cipher);
                }
            } else {
                throw new InvalidKeyException("Key algorithm must be compatible with "+ENCRYPTION_ALGORITHM);
            }
        } else {
            throw new InvalidParameterException("Algorithms in SealedObject and SecretKey must match.");
        }
    }

    public SecretKey generateSecretKey(PrivateKey myKey, PublicKey otherKey) throws Exception {
        if (myKey instanceof DHPrivateKey && otherKey instanceof DHPublicKey) {
            synchronized (agreement) {
                agreement.init(myKey);
                agreement.doPhase(otherKey, true);

                return agreement.generateSecret("AES");
            }
        } else {
            throw new InvalidKeyException("Key algorithms must be compatible with "+EXCHANGE_ALGORITHM);
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
}
