package manualTests;

<<<<<<< HEAD
import command.communicationToUI.CommandWord;
import command.communicationToUI.Operation;
import command.communicationToUI.OperationFinishedListener;
import control.PeerOwner;
=======
import se.chalmers.gdcn.communicationToUI.CommandWord;
import se.chalmers.gdcn.communicationToUI.Operation;
import se.chalmers.gdcn.communicationToUI.OperationFinishedListener;
import se.chalmers.gdcn.control.PeerOwner;
>>>>>>> dev1
import net.tomp2p.peers.Number160;
import net.tomp2p.storage.Data;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
<<<<<<< HEAD
=======
import se.chalmers.gdcn.utils.ByteArray;
>>>>>>> dev1

import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.Semaphore;

/**
 * Created by HalfLeif on 2014-04-08.
 *
 * Test used for making sure that Task results are deserialized correctly from class Data.
 */
public class BinaryTest {

    private PeerOwner client;
    private Random random;

    private volatile boolean success;

    @BeforeMethod
    public void setupMethod(){
        client = new PeerOwner();
        random = new Random(19256387658L);
        success = true;
    }

    @Test
<<<<<<< HEAD
    public void testBytes(){
        final byte[] bytes = generate();
        final Number160 key = Number160.createHash("SomeReallyGoodKey");
        System.out.println("Original: "+print(bytes));
=======
    public void testPrint(){

        byte[] bytes = new byte[2];
        System.out.println(ByteArray.print(bytes));
        assert "0x0000".equals(ByteArray.print(bytes));

        bytes[0] = 15;
        System.out.println(ByteArray.print(bytes));
        assert "0x0f00".equals(ByteArray.print(bytes));

        bytes[1] = 10;
        System.out.println(ByteArray.print(bytes));
        assert "0x0f0a".equals(ByteArray.print(bytes));

        bytes[0] = 127;
        System.out.println(ByteArray.print(bytes));
        assert "0x7f0a".equals(ByteArray.print(bytes));

        bytes[0] = -1;
        System.out.println(ByteArray.print(bytes));
        assert "0xff0a".equals(ByteArray.print(bytes));

        bytes[0] = -128;
        System.out.println(ByteArray.print(bytes));
        assert "0x800a".equals(ByteArray.print(bytes));
    }


    @Test
    public void testBytes(){
        final byte[] bytes = generate();
        final Number160 key = Number160.createHash("SomeReallyGoodKey");
        System.out.println("Original: "+ ByteArray.print(bytes));
>>>>>>> dev1

        assert identical(bytes, bytes);
        assert ! identical(bytes, generate());

        client.start(13678);

        final Semaphore lock = new Semaphore(0);

        client.addListener(new OperationFinishedListener(client, key, CommandWord.PUT) {
            @Override
            protected void operationFinished(Operation operation) {
                System.out.println("Bytes put");
                assert operation.isSuccess();

                client.addListener(new OperationFinishedListener(client, key, CommandWord.GET) {
                    @Override
                    protected void operationFinished(Operation operation2) {
                        System.out.println("Bytes got");
                        success &= operation2.isSuccess();

                        Object result = operation2.getResult();
                        System.out.println("Raw object got: "+result.toString());
                        Data resultData = (Data) result;
                        byte[] raw = resultData.getData();
<<<<<<< HEAD
                        System.out.println("Raw getData: "+print(raw));
=======
                        System.out.println("Raw getData: "+ ByteArray.print(raw));
>>>>>>> dev1

                        success &=  identical(bytes, raw);
                        success &=  Arrays.equals(bytes, raw);
                        //OBS When using the binary constructor in Data, one must use getData rather than getObject!
//                        try {
//                            Object realData = resultData.getObject();
//                            System.out.println("RealData toString "+realData.toString());
//                            byte[] finData = (byte[]) realData;
//                            System.out.println("FinData toString "+finData);
//                        } catch (ClassNotFoundException e) {
//                            e.printStackTrace();
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }

                        lock.release();
                    }
                });

                client.get(key);
            }
        });
        client.put(key, new Data(bytes));

        lock.acquireUninterruptibly();
        assert success;
    }

<<<<<<< HEAD
=======
    @Test
    public void testDouble(){
        System.out.println("NEG. MAX VALUE: "+ -Double.MAX_VALUE);
        System.out.println("MIN VALUE: "+ Double.MIN_VALUE);
    }

>>>>>>> dev1
    private boolean identical(byte[] a, byte[] b){
        if(a.length!=b.length){
            return false;
        }
        for(int ix=0; ix<a.length; ++ix){
            if(a[ix] != b[ix]){
                return false;
            }
        }
        return true;
    }

    private byte[] generate(){
        final int len = 64;
        byte[] bytes = new byte[len];
//        for(int i=0; i<len; ++i){
//            bytes[i] = (byte) random.nextInt(128);
//        }
        random.nextBytes(bytes);
        return bytes;
    }

<<<<<<< HEAD
    private String print(byte[] bytes){
        String nice = "0x";
        for(byte b : bytes){
            nice += Integer.toHexString(b);
        }
        return nice;
    }
=======
>>>>>>> dev1
}
