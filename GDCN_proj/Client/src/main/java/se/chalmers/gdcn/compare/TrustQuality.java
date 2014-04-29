package se.chalmers.gdcn.compare;

/**
 * Created by joakim on 4/29/14.
 */
public class TrustQuality {

    private Trust trust;
    private final double quality;

    public TrustQuality(Trust trust, double quality) {
        this.trust = trust;
        this.quality = quality;
    }

    public TrustQuality(Trust trust) {
        this(trust, Double.MIN_VALUE);
    }

    public Trust getTrust() {
        return trust;
    }

    public void setTrust(Trust trust) {
        this.trust = trust;
    }

    public double getQuality() {
        return quality;
    }
}
