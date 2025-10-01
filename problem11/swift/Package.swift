// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "MatrixProduct",
    products: [
        .executable(name: "MatrixProduct", targets: ["MatrixProduct"])
    ],
    targets: [
        .executableTarget(
            name: "MatrixProduct",
            path: ".",
            sources: ["main.swift", "MatrixProduct.swift"]
        ),
        .testTarget(
            name: "MatrixProductTests",
            dependencies: ["MatrixProduct"],
            path: ".",
            sources: ["MatrixProductTests.swift"]
        )
    ]
)

