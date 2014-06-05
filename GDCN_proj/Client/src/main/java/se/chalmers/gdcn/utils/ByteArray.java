package se.chalmers.gdcn.utils;

import java.io.Serializable;
import java.util.Arrays;

/**
 * An implementation of byte arrays that uses content comparison instead of reference comparison
 * The data of this class has to be treated as an immutable, otherwise behavior with hash code is undefined
 *
 * Created by joakim on 4/13/14.
 */
public class ByteArray implements Serializable {

    private final byte[] data;
    private final int computedHashCode;

    public ByteArray(byte[] data) {
        this.data = data;
        computedHashCode = Arrays.hashCode(data);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ByteArray)) {
            return false;
        }
        ByteArray otherByteArray = (ByteArray) o;
        if (data == otherByteArray.data) {
            return true;
        }
        return Arrays.equals(data, otherByteArray.data);
    }

    @Override
    public int hashCode() {
        return computedHashCode;
    }

    @Override
    public String toString() {
        return print(data);
    }

    public byte[] getData() {
        return data;
    }


    public static String print(byte[] bytes){
        String nice = "0x";
        for(byte b : bytes){
            int small = b & 0x0f;
            int large = (b & 0xf0) >> 4;
            nice += charOfByte(large) + charOfByte(small);
        }
        return nice;
    }

    private static String charOfByte(int b){
        if(b < 0 || b > 15){
            throw new IllegalArgumentException("Byte "+b);
        }
        if(b<10){
            return ""+b;
        } else {
            return Integer.toHexString(b);
        }
    }

}
