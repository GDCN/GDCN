import net.tomp2p.storage.Data;
import network.NetworkMessage;
import org.testng.annotations.Test;

/**
 * Created by Leif on 2014-03-29.
 */
public class MessageTest {

    @Test
    public void encryptDecryptMessage(){
        NetworkMessage message = new NetworkMessage("SomeData", NetworkMessage.Type.REQUEST, 127658817L);

        Data encrypted = message.encrypt();
        NetworkMessage decrypted = NetworkMessage.decrypt(encrypted);

        assert decrypted.getRef().equals(message.getRef());
        assert decrypted.getType() == message.getType();
        assert decrypted.getObject().equals(message.getObject());
    }

}
