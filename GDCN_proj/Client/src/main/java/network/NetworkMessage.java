package network;

import javax.crypto.*;
import java.io.IOException;
import java.io.Serializable;
import java.security.*;
import java.util.Objects;

/**
 * Created by Leif on 2014-03-24.
 *
 * General purpose class used for any message passing.
 */
public class NetworkMessage implements Serializable {

    private final Serializable object;
    private final Type type;
    private static Cipher cipher;
    private static Signature signer;

    static {
        cipher = null;
        signer = null;

        try {
            cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
            e.printStackTrace();
        } catch (NoSuchPaddingException e) {
            //The Java platform is defective, it does not support all required Cipher transformations.
            //See http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
            e.printStackTrace();
        }

        try {
            signer = Signature.getInstance("SHA256withRSA");
        } catch (NoSuchAlgorithmException e) {
            //The Java platform is defective, it does not support all required Signature algorithms.
            //See http://docs.oracle.com/javase/7/docs/api/java/security/Signature.html
            e.printStackTrace();
        }
    }

    public NetworkMessage(Serializable object, Type type) {
        this.object = object;
        this.type = type;
    }

    public Object getObject() {
        return object;
    }

    public Type getType() {
        return type;
    }

    /**
     * Encrypts and signs message using receiving peer's public key
     * @return encrypted message
     */
    public SealedObject encryptAndSign(PublicKey otherKey, PrivateKey myKey) throws IOException, InvalidKeyException, SignatureException {
        SignedObject signedObject = sign(this,myKey);
        return encrypt(signedObject,otherKey);
    }

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

    public static SignedObject sign(Serializable data, PrivateKey key) throws InvalidKeyException, IOException, SignatureException {
        synchronized (signer) {
            return new SignedObject(data,key,signer);
        }
    }

    /**
     * Decrypt message using private key
     * @param data Encrypted message
     * @return Decrypted message
     */
    public static NetworkMessage decryptAndVerify(SealedObject data, PublicKey otherKey, PrivateKey myKey) throws Exception {
        SignedObject signedData = (SignedObject) decrypt(data,myKey);

        if (signedData.verify(otherKey,signer)) {
            return (NetworkMessage) signedData.getObject();
        } else {
            throw new Exception("ERROR! ERROR! Signature did not match.");
        }

    }

    public static Object decrypt(SealedObject data, PrivateKey key) throws InvalidKeyException, ClassNotFoundException, BadPaddingException, IllegalBlockSizeException, IOException {
        synchronized (cipher) {
            cipher.init(Cipher.DECRYPT_MODE,key);

            return data.getObject(cipher);
        }
    }

    public static boolean verify(SignedObject data, PublicKey key) throws SignatureException, InvalidKeyException {
        synchronized (signer) {
            return data.verify(key,signer);
        }
    }

    @Override
    public String toString() {
        return "NetwM{ " + type +
                ", " + object +
                '}';
    }

    public static enum Type {
        REQUEST,
        NO_REPLY
    }


}
