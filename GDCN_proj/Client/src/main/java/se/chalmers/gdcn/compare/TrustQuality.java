package se.chalmers.gdcn.compare;

/**
 * Created by joakim on 4/29/14.
 *
 * Class for storing trust and the quality of a result
 */
public class TrustQuality {

    private final Trust trust;
    private final double quality;
    private final String reason;

    private TrustQuality(Trust trust, double quality, String reason) {
        this.trust = trust;
        this.quality = quality;
        this.reason = reason;
    }

    public static TrustQuality unknown(String reason){
        return new TrustQuality(Trust.UNKNOWN, Double.MIN_VALUE, reason);
    }

    public static TrustQuality deceitful(){
        return new TrustQuality(Trust.DECEITFUL, Double.MIN_VALUE, null);
    }

    public static TrustQuality trustworthy(double quality){
        return new TrustQuality(Trust.TRUSTWORTHY, quality, null);
    }

    public Trust getTrust() {
        return trust;
    }

    public double getQuality() {
        return quality;
    }

    public String getReason() {
        return reason;
    }
}
