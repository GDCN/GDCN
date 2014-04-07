package unitTests;

import network.Crypto;
import network.NetworkMessage;
import org.testng.annotations.Test;

/**
 * Created by Leif on 2014-03-29.
 */
public class MessageTest {

    @Test
    public void encryptDecryptMessage(){
        NetworkMessage message = new NetworkMessage("SomeData", NetworkMessage.Type.REQUEST);

        Object encrypted = message.signAndEncrypt();
        NetworkMessage decrypted = Crypto.decryptAndVerify(encrypted);

        assert decrypted.getType() == message.getType();
        assert decrypted.getObject().equals(message.getObject());
    }

}
