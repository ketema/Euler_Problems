use rust::greatest_product;

#[test]
fn test_greatest_product_4x4() {
    let matrix = vec![
        vec![1, 2, 3, 4],
        vec![5, 6, 7, 8],
        vec![9, 10, 11, 12],
        vec![13, 14, 15, 16],
    ];
    let (result, coords) = greatest_product(&matrix, 4);
    let expected = 43680; // 13*14*15*16
    let expected_coords = vec![(3, 0), (3, 1), (3, 2), (3, 3)];
    assert_eq!(result, expected);
    assert_eq!(coords, expected_coords);
}
