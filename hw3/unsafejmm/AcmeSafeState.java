import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
	int length = value.length();
	long[] array = new long[length];
	for(int i = 0; i < length; i++){
		array[i] = value.get(i);
	}
	return array;
    }

    public void swap(int i, int j) {
	value.getAndDecrement(i);
	value.getAndIncrement(j);
    }
}
