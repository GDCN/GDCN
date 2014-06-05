package se.chalmers.gdcn.network;

import javax.crypto.*;
import java.security.Security;

import org.apache.commons.lang.SerializationUtils;
import org.apache.shiro.crypto.CryptoException;
import org.apache.shiro.crypto.DefaultBlockCipherService;
import org.apache.shiro.crypto.OperationMode;
import org.apache.shiro.crypto.PaddingScheme;
import org.apache.shiro.util.ByteSource;
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
    public final static String AGREEMENT_KEY_ALGORITHM = "DH";
    public final static int AGREEMENT_KEY_SIZE = 1024;

    public final static String ENCRYPTION_ALGORITHM = "AES";

    public final static String SIGNATURE_ALGORITHM = "SHA1withDSA";
    public final static String SIGNATURE_KEY_ALGORITHM = "DSA";
    public final static int SIGNATURE_KEY_SIZE = 1024;

    private final static GCMCipherService cipher;
    private final static KeyAgreement agreement = initAgreement();
    private final static Signature signer = initSigner();

    private final static KeyPairGenerator agreementKeygen = initKeygen(AGREEMENT_ALGORITHM,AGREEMENT_KEY_SIZE);
    private final static KeyPairGenerator signatureKeygen = initKeygen(SIGNATURE_KEY_ALGORITHM,SIGNATURE_KEY_SIZE);

    static {
        Security.addProvider(new BouncyCastleProvider());
        cipher = new GCMCipherService();
    }

    /**
     * Encrypts a serializable object with AES-GCM.
     * @param plaintext The object to be encrypted.
     * @param key The key with which the object should be encrypted.
     * @return The ciphertext
     * @throws InvalidKeyException If the supplied key was incompatible with the encryption algorithm.
     */
    public static byte[] encrypt(Serializable plaintext, SecretKey key) throws InvalidKeyException {
        if (ENCRYPTION_ALGORITHM.equals(key.getAlgorithm())) {
            byte[] plainBytes = SerializationUtils.serialize(plaintext);

            return cipher.encrypt(plainBytes, key.getEncoded()).getBytes();
        } else {
            throw new InvalidKeyException("Key algorithm must be compatible with "+ ENCRYPTION_ALGORITHM);
        }
    }

    /**
     * Decrypts and authenticates a ciphertext with AES-GCM.
     * @param ciphertext The data to be decrypted.
     * @param key The key with which the object should be decrypted and authenticated.
     * @return The decrypted object on successful decryption and authentication, null otherwise.
     * @throws InvalidKeyException If the supplied key was incompatible with the encryption algorithm.
     */
    public static Serializable decrypt(byte[] ciphertext, SecretKey key) throws InvalidKeyException {
        if (ENCRYPTION_ALGORITHM.equals(key.getAlgorithm())) {
            ByteSource plaintext;

            try {
                plaintext = cipher.decrypt(ciphertext,key.getEncoded());
            } catch (CryptoException e) {
                return null;
            }

            return (Serializable) SerializationUtils.deserialize(plaintext.getBytes());
        } else {
            throw new InvalidKeyException("Key algorithm must be compatible with "+ ENCRYPTION_ALGORITHM);
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
        if (AGREEMENT_KEY_ALGORITHM.equals(myKey.getAlgorithm()) && AGREEMENT_KEY_ALGORITHM.equals(otherKey.getAlgorithm())) {
            synchronized (agreement) {
                agreement.init(myKey);
                agreement.doPhase(otherKey, true);

                try {
                    return agreement.generateSecret(ENCRYPTION_ALGORITHM);
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

    private static class GCMCipherService extends DefaultBlockCipherService {
        public GCMCipherService() {
            super(ENCRYPTION_ALGORITHM);
            setMode(OperationMode.GCM);
            setPaddingScheme(PaddingScheme.NONE);
        }
    }
}
