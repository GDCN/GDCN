package se.chalmers.gdcn.network;

import java.io.Serializable;
import java.security.PublicKey;

/**
 * Created by weeeeeew on 2014-04-10.
 */
public class Handshake implements Serializable {
    public final PublicKey agreementKey, signKey;
    public final Phase phase;

    /**
     * Creates a new Handshake in the initial phase, containing the specified keys.
     * @param agreementKey The public key of the key agreement algorithm.
     * @param signKey The public key of the digital signature algorithm.
     */
    public Handshake(PublicKey agreementKey, PublicKey signKey) {
        this(agreementKey, signKey, Phase.INIT);
    }

    private Handshake(PublicKey agreementKey, PublicKey signKey, Phase phase) {
        this.agreementKey = agreementKey;
        this.signKey = signKey;
        this.phase = phase;
    }

    /**
     * Creates a replying handshake, using the specified keys.
     * @param agreementKey The public key of the key agreement algorithm.
     * @param signKey The public key of the digital signature algorithm.
     * @return A new handshake in the next phase.
     */
    public Handshake reply(PublicKey agreementKey, PublicKey signKey) {
        return phase == Phase.INIT ? new Handshake(agreementKey,signKey, Phase.REPLY) : null;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Handshake) {
            Handshake hs = (Handshake) o;
            return this.agreementKey.equals(hs.agreementKey) && this.signKey.equals(hs.signKey) && this.phase == hs.phase;
        }

        return false;
    }

    @Override
    public String toString() {
        return "Handshake{"+
                "\n\tPhase: "+phase+
                "\n\tSignKey: "+signKey+
                "\n\tAgreeKey: "+agreementKey+
                "\n}";
    }

    public enum Phase {
        INIT,
        REPLY
    }
}
